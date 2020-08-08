;;;; bleachers.asd

(asdf:defsystem #:bleachers
  :description "Describe bleachers here"
  :author "ava fox"
  :license  "NPLv1+"
  :version "0.0.1"
  :serial t
  :depends-on (#:cl-blaseball #:cl-blaseball-streaming
	       #:cl-cffi-gtk #:cl-ppcre #:yason
	       #:alexandria #:safe-queue #:json-mop
	       #:str #:with-user-abort #:deploy)
  :components ((:file "package")
	       (:file "util")
               (:file "bleachers"))
  :build-operation "deploy-op"
  :build-pathname "bleachers"
  :entry-point "bleachers:main")
