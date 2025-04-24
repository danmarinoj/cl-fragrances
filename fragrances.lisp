(in-package #:fragrances)

(defun my-getenv (name &optional default)
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))

(defun list-tables (db)
  (mapcar
   #'car
   (sqlite:execute-to-list db "SELECT name FROM sqlite_master WHERE type='table'")))

;; (defun get-raw-materials-table (table-name)
;;   (format NIL "SELECT raw_material FROM ~a" table-name))

;; (defun build-raw-ingredient-query (tables)
;;   (concatenate 'string
;; 	       "SELECT DISTINCT raw_material FROM ("
;; 	       (format NIL "~{~A~^ UNION ~}"
;; 		       (mapcar #'get-raw-materials-table tables))
;; 	       ") AS u"))

;; variables
(defvar *db* (sqlite:connect (my-getenv "FRAGRANCE_PATH" "fragrances.db")))
;; (defvar *completions-list*
;;   (let ((tables (list-tables *db*)))
;;     (let ((raw-materials (sqlite:execute-to-list
;; 			  *db*
;; 			  (build-raw-ingredient-query tables))))
;;       (append tables raw-materials))))

(defun parse-float (value)
  (with-input-from-string (in value)
    (read in)))

;; define prompts
(defun prompt (prompt)
  (string-downcase
   (rl:readline :prompt (format NIL "~c[32m~a>~c[0m " #\ESC prompt #\ESC)
		:add-history T)))

(defun prompt-value (prompt)
  (rl:readline :prompt (format NIL "~c[35m~a:~c[0m " #\ESC prompt #\ESC)
	       :add-history T))

(defun get-formula (formula-name)
  (sqlite:execute-to-list *db* (format NIL "SELECT * FROM ~a" formula-name)))

(defun tabulate (formula-name data column-names totals)
  (let ((table (ascii-table:make-table column-names :header formula-name)))
    (dolist (row data)
      (ascii-table:add-row table row))
    (ascii-table:add-separator table)
    (ascii-table:add-row table totals)
    (ascii-table:display table)))

(defun compute-percentage (formula-tuple total-parts)
  (* 100 (float (/ (cadr formula-tuple) total-parts))))

(defun compute-total-parts (formula)
  (reduce #'(lambda (running-sum formula-tuple) (+ running-sum (cadr formula-tuple)))
	  formula :initial-value 0))

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
	 (let ((accord-total
		 (reduce #'(lambda (x y) (+ x (cadr y)))
			 accord
			 :initial-value 0)))
	   (let ((gcd-proportion (gcd proportion accord-total)))
	     (let ((accord-factor (/ proportion gcd-proportion))
		   (formula-factor (/ accord-total gcd-proportion)))
	       ;; update values in old formula
	       (dolist (formula-tuple (get-formula formula-name))
		 (update formula-name (car formula-tuple) (* formula-factor (cadr formula-tuple))))
	       ;; add the accord
	       (dolist (formula-tuple accord)
		 (insert formula-name (car formula-tuple) (* accord-factor (cadr formula-tuple)))))))))))

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
  (format T "(P)rint, (I)nsert, (U)pdate, (D)elete, Import (A)ccord, (C)alculate~%")
  (let (choice)
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
			    (if (equal choice "c")
				(calculate-menu formula-name)))))))))))

(defun main-menu ()
  (format T "(L)ist formulas, (N)ew formula~%")
  (let (choice)
    (loop while (not (equal choice "q")) do
      (setf choice (prompt "fragrances"))
      (if (equal choice "l")
	  (list-formulas-to-edit)
	  (if (equal choice "n")
	      (new-formula (prompt-value "New formula name")))))))
