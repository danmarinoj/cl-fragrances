(in-package #:fragrances)

;; (defun my-getenv (name &optional default)
;;   (or
;;    #+Allegro (sys:getenv name)
;;    #+CLISP (ext:getenv name)
;;    #+ECL (si:getenv name)
;;    #+SBCL 
;;    #+LISPWORKS (lispworks:environment-variable name)
;;    default))

(defun list-tables (db)
  (mapcar
   #'car
   (sqlite:execute-to-list
    db
    (concatenate 'string
		 "SELECT name FROM sqlite_master WHERE type='table' "
		 "AND name LIKE 'formula_%'"))))

(defun get-raw-materials-table (table-name)
  (format NIL "SELECT raw_material FROM ~a" table-name))

(defun build-raw-ingredient-query (tables)
  (concatenate 'string
	       "SELECT DISTINCT raw_material FROM ("
	       (format NIL "~{~A~^ UNION ~}"
		       (mapcar #'get-raw-materials-table tables))
	       ") AS u"))

(defun parse-float (value)
  (with-input-from-string (in value)
    (read in)))
