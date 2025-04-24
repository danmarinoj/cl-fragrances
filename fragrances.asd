(asdf:defsystem fragrances
  :version "1.0.0"
  :license "GPL"
  :author "Daniel Marino Johnson <daniel@marino-johnson.org>"
  :maintainer "Daniel Marino Johnson <daniel@marino-johnson.org>"
  :description "TUI for designing fragrances"
  :components ((:file "package")
	       (:file "fragrances"))
  :depends-on (:sqlite :cl-ascii-table :cl-readline))
