(in-package #:fragrances)

(defvar *db* (sqlite:connect
	      (concatenate 'string *fragrance-home* "/fragrances.db")))

(defun formula-type (formula-name)
  (let ((columns
	  (mapcar #'cadr (sqlite:execute-to-list *db*
						 (format
						  NIL
						  "PRAGMA table_info(~a)"
						  formula-name)))))
    (if (member "concentration" columns :test #'string=)
	:formula
	:formula-no-c)))

(defun formula-from-db (formula-name)
  (cond ((eq (formula-type formula-name) :formula)
	 (let ((query-result (sqlite:execute-to-list
			      *db*
			      (format
			       NIL
			       (concatenate 'string
					    "SELECT raw_material, concentration, proportion FROM ~a "
					    "ORDER BY proportion DESC")
			       formula-name))))
	   (make-instance
	    'formula
	    :name formula-name
	    :items
	    (mapcar #'(lambda (record) (make-instance 'formula-item
						      :raw-material (car record)
						      :concentration (cadr record)
						      :proportion (caddr record)))
		    query-result))))
	((eq (formula-type formula-name) :formula-no-c)
	 (let ((query-result (sqlite:execute-to-list
			      *db*
			      (format
			       NIL
			       (concatenate 'string
					    "SELECT raw_material, proportion FROM ~a "
					    "ORDER BY proportion DESC")
			       formula-name))))
	   (make-instance
	    'formula-no-c
	    :name formula-name
	    :items
	    (mapcar #'(lambda (record) (make-instance 'formula-item-no-c
						      :raw-material (car record)
						      :proportion (cadr record)))
		    query-result))))))

(defun formula-to-db (formula)
  (let ((formula-name (formula-name formula)))
    (sqlite:execute-non-query (format NIL "DELETE FROM ~a" formula-name))
    (dolist (item (formula-items formula))
      (iud-record item formula-name :operation "i"))))

(defgeneric iud-record (my-formula-item formula-name &key operation))
(defmethod iud-record ((my-formula-item formula-item)
		       formula-name &key operation)
  (cond ((equal operation "i")
	 (sqlite:execute-non-query
	  *db*
	  (format NIL
		  "INSERT INTO ~a (raw_material, concentration, proportion) VALUES (?, ?, ?)"
		  formula-name)
	  (get-raw-material my-formula-item)
	  (get-concentration my-formula-item)
	  (get-proportion my-formula-item)))
	((equal operation "u")
	 (sqlite:execute-non-query
	  *db*
	  (format NIL
		  "UPDATE ~a SET concentration = ~a, proportion = ~a WHERE raw_material = '~a'"
		  formula-name
		  (get-concentration my-formula-item)
		  (get-proportion my-formula-item)
		  (get-raw-material my-formula-item))))
	((equal operation "d")
	 (sqlite:execute-non-query
	  *db*
	  (format NIL
		  "DELETE FROM ~a WHERE raw_material = '~a'"
		  formula-name
		  (get-raw-material my-formula-item)))))
  (formula-from-db formula-name))

(defmethod iud-record ((my-formula-item formula-item-no-c)
		       formula-name &key operation)
  (cond ((equal operation "i")
	 (sqlite:execute-non-query
	  *db*
	  (format NIL
		  "INSERT INTO ~a (raw_material, proportion) VALUES (?, ?)"
		  formula-name)
	  (get-raw-material my-formula-item)
	  (get-proportion my-formula-item)))
	((equal operation "u")
	 (sqlite:execute-non-query
	  *db*
	  (format NIL
		  "UPDATE ~a SET proportion = ~a WHERE raw_material = '~a'"
		  formula-name
		  (get-proportion my-formula-item)
		  (get-raw-material my-formula-item))))
	((equal operation "d")
	 (sqlite:execute-non-query
	  *db*
	  (format NIL
		  "DELETE FROM ~a WHERE raw_material = '~a'"
		  formula-name
		  (get-raw-material my-formula-item)))))
  (formula-from-db formula-name))

(defun new-formula (formula-name)
  (sqlite:execute-non-query
   *db*
   (format NIL "CREATE TABLE ~a (raw_material string, proportion integer)" formula-name)))

(defun disconnect-db ()
  (sqlite:disconnect *db*))
