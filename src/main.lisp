(in-package #:fragrances)

;; variables
(defvar *completions-list*
  (let ((tables (concatenate 'list
			     '("current_inventory" "suggestions")
			     (list-tables *db*))))
    (mapcar #'car (sqlite:execute-to-list
		   *db*
		   (build-raw-ingredient-query tables)))))

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

(defun prompt-value (prompt &key (default NIL))
  (rl:readline :prompt (format NIL "~c[35m~a:~c[0m " #\ESC prompt #\ESC)
	       :add-history T))

(defun list-formulas (on-choose-function)
  (format T "Formulas:~%")
  (let ((counter 0)
	(tables (list-tables *db*)))
    (dolist (formula-id tables)
      (format T "(~a) ~a~%" counter (decode-formula-name formula-id))
      (incf counter))
    (format T "~%")
    (let ((choice (prompt-value "Select formula")))
      (if (equal choice "q")
	  ()
	  (let ((formula-id (nth (parse-integer choice) tables)))
	    (funcall on-choose-function formula-id))))))

(defun list-formulas-to-edit ()
  (list-formulas
   #'(lambda (formula-id)
       (modify-data-menu formula-id))))

(defun experiment-next-menu (parent base-formula branches)
  (let ((experiments-hash (get-experiments-hash branches)))
    (dolist (experiment-id branches)
      (format T "(~a) ~a~%"
	      experiment-id
	      (experiment-name (gethash experiment-id experiments-hash))))
    (format T "~%~%(N)ew experiment, (A)ccept result~%")
    (let ((choice (prompt (encode-formula-name parent))))
      (cond ((equal choice "n")
	     (experiment-create-menu parent base-formula))
	    ((equal choice "a")
	     (let ((source-formula (formula-from-db (encode-formula-name parent)))
		   (target-formula (formula-from-db base-formula)))
	       (setf (formula-items target-formula)
		     (formula-items source-formula))
	       (formula-to-db target-formula)))
	    ((equal choice "q") NIL)
	    (T
	     (let ((experiment
		     (gethash (parse-integer choice)
			      experiments-hash)))
	       (display-experiment-menu experiment)))))))

(defun display-experiment-menu (my-experiment)
  (format T "~a" my-experiment)
  (experiment-next-menu (experiment-id my-experiment)
			(experiment-base-formula my-experiment)
			(experiment-branches my-experiment)))

(defun experiment-create-menu (parent base-formula)
  (let ((name (prompt-value "Experiment name"))
	(hypothesis (prompt-value "Hypothesis"))
	experiment-id)
    (setf experiment-id
	  (new-experiment name parent base-formula NIL hypothesis))
    (modify-data-menu (encode-formula-name experiment-id))
    ;; update conclusion and parent branches
    (let ((conclusion (prompt-value "Conclusion")))
      (update-conclusion experiment-id conclusion))
    (if parent
	(update-branches parent experiment-id))
    ;; proceed to next menu
    (experiment-next-menu experiment-id base-formula NIL)))

(defun experiments-top-level (formula-id)
  (destructuring-bind (experiment-ids experiments-hash)
      (list-experiments-by-formula formula-id)
    (format T "Experiments:~%")
    (dolist (id experiment-ids)
      (format T "(~a) ~a~%" id (experiment-name (gethash id experiments-hash))))
    (format T "~%")
    (let ((choice (prompt-value "Select experiment or create new (N)")))
      (cond ((equal choice "q")
	     ())
	    ((equal choice "n")
	     (experiment-create-menu NIL formula-id))
	    (T
	     (let ((experiment
		     (gethash (parse-integer choice)
			      experiments-hash)))
	       (display-experiment-menu experiment)))))))

(defun upsert-menu (formula-id operation)
  (let (item
	(formula-type (formula-type formula-id)))
    (setf item
	  (cond ((eq formula-type :formula)
		 (let ((raw-material (prompt-value "Raw material"))
		       (concentration (parse-integer (prompt-value "Concentration")))
		       (proportion (parse-integer (prompt-value "Proportion"))))
		   (make-instance 'formula-item
				  :raw-material raw-material
				  :concentration concentration
				  :proportion proportion)))
		((eq formula-type :formula-no-c)
		 (let ((raw-material (prompt-value "Raw material"))
		       (proportion (parse-integer (prompt-value "Proportion"))))
		   (make-instance 'formula-item-no-c
				  :raw-material raw-material
				  :proportion proportion)))))
    (iud-record item formula-id :operation operation)))

(defun delete-menu (formula-id)
  (let ((raw-material (prompt-value "Raw material")))
    (iud-record (make-instance 'formula-item-no-c
			       :raw-material raw-material)
		formula-id :operation "d")))

(defun import-accord-menu (formula-id)
  (list-formulas
   #'(lambda (accord-name)
       (let ((proportion (parse-integer (prompt-value "Proportion")))
	     (accord (formula-from-db accord-name)))
	 (destructuring-bind (accord-factor formula-factor)
	     (import-accord-factors accord proportion)
	   ;; update values in old formula
	   (dolist (formula-tuple (formula-from-db formula-id))
	     (update formula-id (car formula-tuple) (* formula-factor (cadr formula-tuple))))
	   ;; add the accord
	   (dolist (formula-tuple accord)
	     (insert formula-id (car formula-tuple) (* accord-factor (cadr formula-tuple)))))))))

(defun bulk-import-menu (formula)
  (let (quit line)
    (loop while (not quit) do
      (let ((line (rl:readline)))
	(if (equal line "q")
	    (setf quit T)
	    (destructuring-bind (raw-material quantity) (uiop:split-string line :separator ",")
	      (setf formula (cons `(,raw-material ,(parse-float quantity)) formula))))))
    ;; add the records
    (formula-to-db formula)
    formula))

(defun calculate-menu (formula)
  (let ((raw-material (prompt-value "Raw material"))
	(amount (parse-float (prompt-value "Amount"))))
    (let* ((proportion (get-proportion
			(car
			 (remove-if-not
			  #'(lambda (item) (equal raw-material (get-raw-material item)))
			  (formula-items formula)))))
	   (unit (/ amount proportion))
	   (formula-with-mass (formula-with-mass-from-formula formula unit)))
      (format T "~a" formula-with-mass))))

(defun modify-data-menu (formula-id)
  (let ((help-message (concatenate 'string
				   "(P)rint, (E)xperiment,~%"
				   "(I)nsert, (U)pdate, (D)elete, Import (A)ccord,~%"
				   "(B)ulk import, (C)alculate, (H)elp~%"))
	(formula (formula-from-db formula-id))
	choice)
    (format T help-message)
    (loop while (not (equal choice "q")) do
      (progn
	(setf choice (prompt (decode-formula-name formula-id)))
	(cond ((equal choice "p")
	       (format T "~a" formula))
	      ((equal choice "e")
	       (experiments-top-level formula-id))
	      ((equal choice "i")
	       (setf formula (upsert-menu formula-id choice)))
	      ((equal choice "u")
	       (setf formula (upsert-menu formula-id choice)))
	      ((equal choice "d")
	       (setf formula (delete-menu formula-id)))
	      ((equal choice "a")
	       (import-accord-menu formula-id))
	      ((equal choice "b")
	       (setf formula (bulk-import-menu formula)))
	      ((equal choice "c")
	       (calculate-menu formula))
	      ((equal choice "h")
	       (format T help-message)))))))

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
      (cond ((equal choice "l")
	     (list-formulas-to-edit))
	    ((equal choice "n")
	     (let ((formula-name (prompt-value "New formula name"))
		   formula-id)
	       (setf formula-id (new-formula formula-name))
	       (modify-data-menu formula-id)))
	    ((equal choice "d")
	     (calculate-dilution-menu)))))
  (disconnect-db))
