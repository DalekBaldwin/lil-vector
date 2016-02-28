(in-package :cl-user)

(defpackage :lil-vector
  (:use :cl))

(in-package :lil-vector)

(defparameter *system-directory*
  (make-pathname
   :directory
   (pathname-directory
    (asdf:system-definition-pathname "lil-vector"))))
