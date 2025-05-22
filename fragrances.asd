(asdf:defsystem fragrances
  :version "1.0.0"
  :license "GPL"
  :author "Daniel Marino Johnson <daniel@marino-johnson.org>"
  :maintainer "Daniel Marino Johnson <daniel@marino-johnson.org>"
  :description "TUI for designing fragrances"
  :depends-on (:sqlite :cl-ascii-table :cl-readline :str :uiop :cl-base64 :split-sequence)
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "classes")
     (:file "db")
     (:file "utils")
     (:file "calculations")
     (:file "main")))))
