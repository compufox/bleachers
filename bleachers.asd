;;;; bleachers.asd

(asdf:defsystem #:bleachers
  :description "Describe bleachers here"
  :author "ava fox"
  :license  "NPLv1+"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-blaseball #:cl-blaseball-streaming
	       #:cl-cffi-gtk #:cl-ppcre #:yason
	       #:alexandria)
  :components ((:file "package")
               (:file "bleachers")))
