(in-package :cl-user)

(defpackage :lil-vector
  (:use :cl :alexandria)
  (:export #:empty-pbvt
           #:lookup-pbvt
           #:static-pbvt
           #:update-pbvt
           #:fold-left-pbvt
           #:fold-left*-pbvt
           #:fold-right-pbvt
           #:fold-right*-pbvt
           #:map-pbvt
           #:collect-all
           #:conj-pbvt
           #:pop-pbvt
           #:divide-pbvt
           #:equal-pbvt))

(in-package :lil-vector)

(defparameter *system-directory*
  (make-pathname
   :directory
   (pathname-directory
    (asdf:system-definition-pathname "lil-vector"))))
