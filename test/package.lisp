(in-package :cl-user)

(defpackage :lil-vector-test
  (:use :cl :lil-vector :stefil :alexandria :check-it)
  (:export
   #:test-all))

(in-package :lil-vector-test)

(defparameter *system-directory* lil-vector::*system-directory*)
