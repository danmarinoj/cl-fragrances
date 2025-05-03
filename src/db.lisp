(in-package #:fragrances)

(defun formula-from-db (db formula-name)
  (let ((query-result (sqlite:execute-to-list
			  db
			  (format NIL
				  "SELECT raw_material, concentration, proportion FROM ~a"
				  formula-name))))
    (make-instance 'formula
		   :name formula-name
		   :items
		   (mapcar #'(lambda (record) (make-instance 'formula-item
							     :raw-material (car record)
							     :concentration (cadr record)
							     :proportion (caddr record)))
			   query-result))))

(defgeneric iud-record (my-formula-item formula-name db &key operation))
(defmethod iud-record ((my-formula-item formula-item)
		       formula-name db &key operation)
  (cond ((equal operation "i")
	 (sqlite:execute-non-query
	  db
	  (format NIL
		  "INSERT INTO ~a (raw_material, concentration, proportion) VALUES (?, ?, ?)"
		  formula-name)
	  (get-raw-material my-formula-item)
	  (get-concentration my-formula-item)
	  (get-proportion my-formula-item)))
	((equal operation "u")
	 (sqlite:execute-non-query
	  db
	  (format NIL
		  "UPDATE ~a SET concentration = ~a, proportion = ~a WHERE raw_material = '~a'"
		  formula-name
		  (get-concentration my-formula-item)
		  (get-proportion my-formula-item)
		  (get-raw-material my-formula-item))))
	((equal operation "d")
	 (sqlite:execute-non-query
	  db
	  (format NIL
		  "DELETE FROM ~a WHERE raw_material = '~a'"
		  formula-name
		  (get-raw-material my-formula-item)))))
  (formula-from-db db formula-name))

(defun new-formula (db formula-name)
  (sqlite:execute-non-query
   db
   (format NIL "CREATE TABLE ~a (raw_material string, proportion integer)" formula-name)))
