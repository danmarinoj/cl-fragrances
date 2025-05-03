(in-package #:fragrances)

(defun compute-percentage (formula-tuple total-parts)
  (* 100 (float (/ (cadr formula-tuple) total-parts))))

(defun import-accord-factors (accord proportion)
  (let ((accord-total
	  (reduce #'(lambda (running-sum x) (+ running-sum (cadr x)))
		  accord
		  :initial-value 0)))
    (let ((gcd-proportion (gcd proportion accord-total)))
      ;; first is the accord factor, the second is the formula factor
      (list (/ proportion gcd-proportion)
	    (/ accord-total gcd-proportion)))))

(defun calculate-dilution (total-mass dilution-percent)
  (let ((raw-material-mass (float (* total-mass (/ dilution-percent 100)))))
    (list raw-material-mass (- total-mass raw-material-mass))))

(defun integer-proportions-from-fractions (formula)
  (let ((common-denominator
	  (reduce #'(lambda (running-lcm formula-tuple)
		      (lcm running-lcm (denominator (cadr formula-tuple))))
		  formula :initial-value 1)))
    (mapcar #'(lambda (formula-tuple)
		(list (car formula-tuple)
		      (* common-denominator (cadr formula-tuple))))
	    formula)))
