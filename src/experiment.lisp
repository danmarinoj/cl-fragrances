(in-package #:fragrances)

(defvar *fragrance-home* (or (sb-unix::posix-getenv "FRAGRANCE_HOME") "."))

(defun query-sexp (sexp key)
  (cdar
   (remove-if-not #'(lambda (x)
		      (eq key (car x)))
		  sexp)))

(defmethod to-sexp ((my-experiment experiment))
  `((:name . ,(experiment-name my-experiment))
    (:id . ,(experiment-id my-experiment))
    (:parent . ,(experiment-parent my-experiment))
    (:base-formula . ,(experiment-base-formula my-experiment))
    (:branches . ,(mapcar #'to-sexp (experiment-branches my-experiment)))
    (:hypothesis . ,(experiment-hypothesis my-experiment))
    (:conclusion . ,(experiment-conclusion my-experiment))))
    
(defun experiment-from-sexp (experiment-sexp)
  (let ((id (query-sexp :id)))
    (make-instance 'experiment
		   :name (query-sexp experiment-sexp :name)
		   :id (if id id "this") ;serialize
		   :parent (query-sexp experiment-sexp :parent)
		   :base-formula (query-sexp experiment-sexp :base-formula)
		   :branches (mapcar #'experiment-from-sexp
				     (query-sexp experiment-sexp :branches))
		   :hypothesis (query-sexp experiment-sexp :hypothesis)
		   :conclusion (query-sexp experiment-sexp :conclusion))))

(defmethod save ((my-experiment experiment))
  (let ((file-name (format NIL
			   "~a/experiments/~a.lisp"
			   *fragrance-home*
			   (experiment-id my-experiment))))
    (with-open-file (stream file-name :direction :output :if-exists :supersede)
      (format stream "~S" (to-sexp experiment)))))

(defun load-experiment (experiment-id)
  (let ((file-name (format NIL
			   "~a/experiments/~a.lisp"
			   *fragrance-home* experiment-id)))
    (with-open-file (stream file-name :if-does-not-exist NIL)
      (experiment-from-sexp (read stream)))))


