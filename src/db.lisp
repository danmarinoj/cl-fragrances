(in-package #:fragrances)

(defvar *db* (sqlite:connect (or
			      (sb-unix::posix-getenv "FRAGRANCE_DB")
			      "fragrances.db")))

(defun formula-type (formula-id)
  (let ((columns
	  (mapcar #'cadr (sqlite:execute-to-list *db*
						 (format
						  NIL
						  "PRAGMA table_info(~a)"
						  formula-id)))))
    (if (member "concentration" columns :test #'string=)
	:formula
	:formula-no-c)))

(defun formula-from-db (formula-id)
  (cond ((eq (formula-type formula-id) :formula)
	 (let ((query-result (sqlite:execute-to-list
			      *db*
			      (format
			       NIL
			       (concatenate 'string
					    "SELECT raw_material, concentration, proportion FROM ~a "
					    "ORDER BY proportion DESC")
			       formula-id))))
	   (make-instance
	    'formula
	    :id formula-id
	    :items
	    (mapcar #'(lambda (record) (make-instance 'formula-item
						      :raw-material (car record)
						      :concentration (cadr record)
						      :proportion (caddr record)))
		    query-result))))
	((eq (formula-type formula-id) :formula-no-c)
	 (let ((query-result (sqlite:execute-to-list
			      *db*
			      (format
			       NIL
			       (concatenate 'string
					    "SELECT raw_material, proportion FROM ~a "
					    "ORDER BY proportion DESC")
			       formula-id))))
	   (make-instance
	    'formula-no-c
	    :id formula-id
	    :items
	    (mapcar #'(lambda (record) (make-instance 'formula-item-no-c
						      :raw-material (car record)
						      :proportion (cadr record)))
		    query-result))))))

(defun formula-to-db (formula)
  (let ((formula-id (formula-id formula)))
    (sqlite:execute-non-query *db* (format NIL "DELETE FROM ~a" formula-id))
    (dolist (item (formula-items formula))
      (iud-record item formula-id :operation "i"))))

(defgeneric iud-record (my-formula-item formula-id &key operation))
(defmethod iud-record ((my-formula-item formula-item)
		       formula-id &key operation)
  (cond ((equal operation "i")
	 (sqlite:execute-non-query
	  *db*
	  (format NIL
		  "INSERT INTO ~a (raw_material, concentration, proportion) VALUES (?, ?, ?)"
		  formula-id)
	  (get-raw-material my-formula-item)
	  (get-concentration my-formula-item)
	  (get-proportion my-formula-item)))
	((equal operation "u")
	 (sqlite:execute-non-query
	  *db*
	  (format NIL
		  "UPDATE ~a SET concentration = ~a, proportion = ~a WHERE raw_material = '~a'"
		  formula-id
		  (get-concentration my-formula-item)
		  (get-proportion my-formula-item)
		  (get-raw-material my-formula-item))))
	((equal operation "d")
	 (sqlite:execute-non-query
	  *db*
	  (format NIL
		  "DELETE FROM ~a WHERE raw_material = '~a'"
		  formula-id
		  (get-raw-material my-formula-item)))))
  (formula-from-db formula-id))

(defmethod iud-record ((my-formula-item formula-item-no-c)
		       formula-id &key operation)
  (cond ((equal operation "i")
	 (sqlite:execute-non-query
	  *db*
	  (format NIL
		  "INSERT INTO ~a (raw_material, proportion) VALUES (?, ?)"
		  formula-id)
	  (get-raw-material my-formula-item)
	  (get-proportion my-formula-item)))
	((equal operation "u")
	 (sqlite:execute-non-query
	  *db*
	  (format NIL
		  "UPDATE ~a SET proportion = ~a WHERE raw_material = '~a'"
		  formula-id
		  (get-proportion my-formula-item)
		  (get-raw-material my-formula-item))))
	((equal operation "d")
	 (sqlite:execute-non-query
	  *db*
	  (format NIL
		  "DELETE FROM ~a WHERE raw_material = '~a'"
		  formula-id
		  (get-raw-material my-formula-item)))))
  (formula-from-db formula-id))

(defun new-formula (formula-name)
  (let ((formula-id (encode-formula-name formula-name)))
    (sqlite:execute-non-query
     *db*
     (format NIL
	     "CREATE TABLE ~a (raw_material string, concentration integer, proportion integer)"
	     formula-id))
    formula-id))

(defun ctas-parent-query (experiment-id parent base-formula)
  (let ((parent-table
	  (if parent
	      (encode-formula-name parent)
	      base-formula)))
    (format NIL "CREATE TABLE ~a AS SELECT * FROM ~a"
	    (encode-formula-name experiment-id)
	    parent-table)))

(defun new-experiment (name parent base-formula branches hypothesis)
  (let ((columns '("name" "parent" "base_formula"
		   "branches" "hypothesis"))
	experiment-id)
    (sqlite:execute-non-query
     *db*
     (format NIL
	     "INSERT INTO experiments (~{~a~^, ~}) VALUES (?, ?, ?, ?, ?)"
	     columns)
     name parent base-formula
     (format NIL "~{~a~^,~}" branches)
     hypothesis)
    (setf experiment-id (sqlite:last-insert-rowid *db*))

    ;; populate formula from parent
    (sqlite:execute-non-query
     *db*
     (ctas-parent-query experiment-id parent base-formula))

    ;; only set this if its a top-level formula
    (if (not parent)
	(sqlite:execute-non-query
	 *db*
	 "INSERT INTO experiments_by_table (tbl_name, experiment_id) VALUES (?, ?)"
	 base-formula experiment-id))
    experiment-id))

(defun update-conclusion (experiment-id conclusion)
  (sqlite:execute-non-query
   *db*
   "UPDATE experiments SET conclusion = ? WHERE rowid = ?"
   conclusion experiment-id))

(defun select-experiment-query ()
  (let ((columns '("name" "parent" "base_formula"
		   "branches" "hypothesis" "conclusion")))
    (format NIL "SELECT ~{~a~^, ~} FROM experiments WHERE rowid = ?"
	    columns)))

(defun parse-branches (branches-str)
  (if (or (not branches-str)
	  (string= branches-str ""))
      NIL
      (mapcar #'parse-integer
	      (split-sequence:split-sequence #\, branches-str))))

(defmethod print-object ((my-experiment experiment) stream)
  (format stream "~c[33m~a~%Variation on ~a~%~a~%~%Hypothesis:~c[0m ~a~%~%"
	  #\ESC
	  (experiment-name my-experiment)
	  (decode-formula-name (experiment-base-formula my-experiment))
	  (let ((parent (experiment-parent my-experiment)))
	    (if parent (format stream "Parent experiment: ~a" parent) "root experiment"))
	  #\ESC
	  (experiment-hypothesis my-experiment))
  (format stream "~a" (formula-from-db
		       (encode-formula-name (experiment-id my-experiment))))
  (format stream "~%~c[33mConclusion:~c[0m ~a~%~c[33mBranches:~c[0m~%"
	  #\ESC
	  #\ESC
	  (experiment-conclusion my-experiment)
	  #\ESC
	  #\ESC))

(defun experiment-from-db (experiment-id)
  (let ((query-result (car
		       (sqlite:execute-to-list
			*db*
			(select-experiment-query)
			experiment-id))))
    (make-instance 'experiment
		   :name (nth 0 query-result)
		   :id experiment-id
		   :parent (nth 1 query-result)
		   :base-formula (nth 2 query-result)
		   :branches (parse-branches
			      (format NIL "~a" (nth 3 query-result)))
		   :hypothesis (nth 4 query-result)
		   :conclusion (nth 5 query-result))))

(defun get-experiments-hash (experiment-ids)
  (let ((experiments-hash (make-hash-table)))
    (dolist (experiment-id experiment-ids)
      (setf (gethash experiment-id experiments-hash)
	    (experiment-from-db experiment-id)))
    experiments-hash))

(defun list-experiments-by-formula (formula-id)
  (let ((experiment-ids
	  (mapcar #'car
		  (sqlite:execute-to-list
		   *db*
		   (concatenate
		    'string
		    "SELECT experiment_id FROM experiments_by_table WHERE tbl_name = ? "
		    "ORDER BY experiment_id ASC")
		   formula-id))))
    (list experiment-ids
	  (get-experiments-hash experiment-ids))))

(defun update-branches (experiment-id child)
  (let ((current-branches (sqlite:execute-single
			   *db*
			   "SELECT branches FROM experiments WHERE rowid = ?"
			   experiment-id))
	updated-branches)
    (setf updated-branches
	  (if (and current-branches
		   (not (equal current-branches "")))
	      (format NIL "~a,~a" current-branches child)
	      child))
    (sqlite:execute-non-query *db*
			      "UPDATE experiments SET branches = ? WHERE rowid = ?"
			      updated-branches experiment-id)))

(defun disconnect-db ()
  (sqlite:disconnect *db*))
