;;;; package.lisp

(defpackage #:bleachers
  (:use #:cl #:gobject #:gtk #:with-user-abort)
  (:export :main))

(in-package #:bleachers)

;; go ahead and make sure the resources folder gets copied
;;  when the app gets built
(deploy:define-resource-directory bleachers "resources")
