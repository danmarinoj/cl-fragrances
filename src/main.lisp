(in-package #:fragrances)

;; variables
(defvar *db* (sqlite:connect (my-getenv "FRAGRANCE_PATH" "fragrances.db")))
(defvar *completions-list*
  (let ((tables (list-tables *db*)))
    (let ((raw-materials (mapcar #'car (sqlite:execute-to-list
					*db*
					(build-raw-ingredient-query tables)))))
      raw-materials)))

;; setup completion
(defun custom-complete (text start end)
  (let ((els (remove-if-not (lambda (it)
                                         (str:starts-with? text it))
                                       *completions-list*)))
    (if (cdr els)
        (cons (str:prefix els) els)
        els)))
(rl:register-function :complete #'custom-complete)

;; define prompts
(defun prompt (prompt)
  (format *query-io* (format NIL "~c[32m~a>~c[0m " #\ESC prompt #\ESC))
  (force-output *query-io*)
  (string-downcase
   (read-line *query-io*)))

(defun prompt-value (prompt)
  (rl:readline :prompt (format NIL "~c[35m~a:~c[0m " #\ESC prompt #\ESC)
	       :add-history T))

(defun get-formula (formula-name)
  (sqlite:execute-to-list *db* (format NIL "SELECT * FROM ~a" formula-name)))

(defun print-formula (formula-name)
  (let ((formula (get-formula formula-name)))
    (let* ((total-parts (compute-total-parts formula))
	   (formula-with-percentages (mapcar
				      #'(lambda (formula-tuple)
					  (reverse
					;add in the percentage
					   (cons (compute-percentage formula-tuple total-parts)
						 (reverse formula-tuple))))
				      formula)))
      (tabulate formula-name formula-with-percentages
		'("Raw Material" "Proportion" "Percentage")
		(list "Total" total-parts 100)))))

(defun list-formulas (on-choose-function)
  (format T "Formulas:~%")
  (let ((counter 0)
	(tables (list-tables *db*)))
    (dolist (table tables)
      (format T "(~a) ~a~%" counter table)
      (incf counter))
    (format T "~%")
    (let ((choice (prompt-value "Select formula")))
      (if (equal choice "q")
	  ()
	  (let ((formula-name (nth (parse-integer choice) tables)))
	    (funcall on-choose-function formula-name))))))

(defun list-formulas-to-edit ()
  (list-formulas
   #'(lambda (formula-name)
       (modify-data-menu formula-name))))

(defun new-formula (formula-name)
  (sqlite:execute-non-query
   *db*
   (format NIL "CREATE TABLE ~a (raw_material string, proportion integer)" formula-name))
  (modify-data-menu formula-name))

(defun insert (formula-name raw-material proportion)
  (sqlite:execute-non-query
   *db*
   (format NIL "INSERT INTO ~a (raw_material, proportion) VALUES (?, ?)" formula-name)
   raw-material proportion))

(defun insert-menu (formula-name)
  (let ((raw-material (prompt-value "Raw material"))
	(proportion (prompt-value "Proportion")))
    (insert formula-name raw-material proportion)))

(defun update (formula-name raw-material proportion)
  (sqlite:execute-non-query
   *db*
   (format NIL "UPDATE ~a SET proportion = ~a WHERE raw_material = '~a'"
	   formula-name proportion raw-material)))

(defun update-menu (formula-name)
  (let ((raw-material (prompt-value "Raw material"))
	(proportion (prompt-value "Proportion")))
    (update formula-name raw-material proportion)))

(defun delete-menu (formula-name)
  (let ((raw-material (prompt-value "Raw material")))
    (sqlite:execute-non-query *db*
     (format NIL "DELETE FROM ~a WHERE raw_material = '~a'" formula-name raw-material))))

(defun import-accord-menu (formula-name)
  (list-formulas
   #'(lambda (accord-name)
       (let ((proportion (parse-integer (prompt-value "Proportion")))
	     (accord (get-formula accord-name)))
	 (destructuring-bind (accord-factor formula-factor)
	     (import-accord-factors accord proportion)
	   ;; update values in old formula
	   (dolist (formula-tuple (get-formula formula-name))
	     (update formula-name (car formula-tuple) (* formula-factor (cadr formula-tuple))))
	   ;; add the accord
	   (dolist (formula-tuple accord)
	     (insert formula-name (car formula-tuple) (* accord-factor (cadr formula-tuple)))))))))

(defun bulk-import-menu (formula-name)
  (let (formula quit line)
    (loop while (not quit) do
      (let ((line (rl:readline)))
	(if (equal line "q")
	    (setf quit T)
	    (destructuring-bind (raw-material quantity) (uiop:split-string line :separator ",")
	      (setf formula (cons `(,raw-material ,(parse-float quantity)) formula))))))
    ;; add the records
    (dolist (formula-tuple (integer-proportions-from-fractions (reverse formula)))
      (insert formula-name (car formula-tuple) (cadr formula-tuple)))))

(defun calculate-menu (formula-name)
  (let ((raw-material (prompt-value "Raw material"))
	(amount (parse-float (prompt-value "Amount")))
	(formula (get-formula formula-name)))
    (let* ((proportion (sqlite:execute-single
			*db*
			(format NIL "SELECT proportion FROM ~a WHERE raw_material = ?" formula-name)
			raw-material))
	   (unit (/ amount proportion))
	   (formula-with-mass (mapcar #'(lambda (formula-tuple)
					  (reverse (cons
					;add in the number of grams
						    (* unit (cadr formula-tuple))
						    (reverse formula-tuple))))
				      formula))
	   (total-parts (compute-total-parts formula)))
      (tabulate formula-name formula-with-mass
		'("Raw Material" "Proportion" "Mass (g)")
		(list "Total" total-parts (* total-parts unit))))))

(defun modify-data-menu (formula-name)
  (let ((help-message (concatenate 'string
				   "(P)rint, (I)nsert, (U)pdate, (D)elete, Import (A)ccord,~%"
				   "(B)ulk import, (C)alculate, (H)elp~%"))
	choice)
    (format T help-message)
    (loop while (not (equal choice "q")) do
      (progn
	(setf choice (prompt formula-name))
	(if (equal choice "p")
	    (print-formula formula-name)
	    (if (equal choice "i")
		(insert-menu formula-name)
		(if (equal choice "u")
		    (update-menu formula-name)
		    (if (equal choice "d")
			(delete-menu formula-name)
			(if (equal choice "a")
			    (import-accord-menu formula-name)
			    (if (equal choice "b")
				(bulk-import-menu formula-name)
				(if (equal choice "c")
				    (calculate-menu formula-name)
				    (if (equal choice "h")
					(format T help-message)))))))))))))

(defun calculate-dilution-menu ()
  (let ((total-mass (parse-float (prompt-value "Total mass")))
	(dilution-percent (parse-integer (prompt-value "Dilution percent"))))
    (destructuring-bind (raw-material-mass alcohol-mass)
	(calculate-dilution total-mass dilution-percent)
      (format T "Mass of raw material: ~a~%" raw-material-mass)
      (format T "Mass of alcohol: ~a~%" alcohol-mass))))      

(defun main-menu ()
  (format T "(L)ist formulas, (N)ew formula, Calulate (D)ilution~%")
  (let (choice)
    (loop while (not (equal choice "q")) do
      (setf choice (prompt "fragrances"))
      (if (equal choice "l")
	  (list-formulas-to-edit)
	  (if (equal choice "n")
	      (new-formula (prompt-value "New formula name"))
	      (if (equal choice "d")
		  (calculate-dilution-menu))))))
  (sqlite:disconnect *db*))
