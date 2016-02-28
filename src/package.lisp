(in-package :cl-user)

(defpackage :lil-vector
  (:use :cl)
  (:export #:empty-pbvt
           #:lookup-pbvt
           #:static-pbvt
           #:update-pbvt
           #:map-pbvt
           #:conj-pbvt
           #:pop-pbvt))

(in-package :lil-vector)

(defparameter *system-directory*
  (make-pathname
   :directory
   (pathname-directory
    (asdf:system-definition-pathname "lil-vector"))))
