(in-package #:fragrances)

(defclass formula-item ()
  ((raw-material :accessor get-raw-material
		 :initarg :raw-material
		 :type string)
   (concentration :accessor get-concentration
		  :initarg :concentration
		  :initform 20
		  :type integer)
   (proportion :accessor get-proportion
	       :initarg :proportion
	       :initform 1		;for delete case
	       :type integer)))

(defclass formula ()
  ((name :accessor formula-name
	 :initarg :name
	 :type string)
   (items :accessor formula-items
	  :initarg :items
	  :type list)
   (percentages :accessor formula-percentages
		:initform (make-hash-table :test 'equal))
   (aromatic-percentages :accessor formula-aromatic-percentages
			 :initform (make-hash-table :test 'equal))))

(defgeneric compute-total-parts (my-formula))
(defmethod compute-total-parts ((my-formula formula))
  (reduce #'(lambda (running-sum item) (+ running-sum (get-proportion item)))
	  (formula-items my-formula) :initial-value 0))

(defmethod initialize-instance :after ((my-formula formula) &key)
  (let ((total-parts (compute-total-parts my-formula)))
    (dolist (item (formula-items my-formula))
      (let ((percentage (* 100 (float (/ (get-proportion item) total-parts)))))
	;; set the percentages in the hash table
	(setf (gethash (get-raw-material item) (formula-percentages my-formula))
	      percentage)
	;; set the aromatic percentages in the hash table
	(setf (gethash (get-raw-material item) (formula-aromatic-percentages my-formula))
	      (* percentage (/ (get-concentration item) 100)))))))

(defun round-columns (row columns-to-round)
  (let ((outrow (copy-list row)))
    (dolist (index columns-to-round)
      (setf (nth index outrow)
	    (format NIL "~,1f" (nth index outrow))))
    outrow))	    

(defun tabulate (formula-name data column-names columns-to-round stream)
  (let ((table (ascii-table:make-table column-names :header formula-name))
	(totals (loop for i from 2 to (length column-names) collect 0)))
    (dolist (row data)
      (ascii-table:add-row table
			   (round-columns row columns-to-round))
      (setf totals (mapcar #'+ totals (cdr row))))
    (ascii-table:add-separator table)
    (setf totals (cons "Total" totals))
    (ascii-table:add-row table
			 (round-columns totals columns-to-round))
    (ascii-table:display table stream)))

(defmethod print-object ((my-formula formula) stream)
  (let ((data (mapcar #'(lambda (item)
			  (let ((raw-material (get-raw-material item)))
			    (list raw-material
				  (get-concentration item)
				  (get-proportion item)
				  (gethash raw-material (formula-percentages my-formula))
				  (gethash raw-material (formula-aromatic-percentages my-formula)))))
		      (formula-items my-formula))))
    (tabulate (formula-name my-formula)
	      data
	      '("Raw Material" "@" "Proportion" "%" "% aromatic")
	      '(3 4)
	      stream)))
