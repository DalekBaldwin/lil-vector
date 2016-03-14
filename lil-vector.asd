;;;; lil-vector.asd

(defpackage :lil-vector-system
  (:use :cl :asdf))
(in-package :lil-vector-system)

(defsystem :lil-vector
  :name "lil-vector"
  :serial t
  :components
  ((:static-file "lil-vector.asd")
   (:module :src
            :components ((:file "package")
                         (:file "pbvt"))
            :serial t))
  :depends-on (:lisp-interface-library)
  :in-order-to ((test-op (load-op :lil-vector-test)))
  :perform (test-op :after (op c)
                    (funcall
                     (intern #.(string '#:run-all-tests)
                             :lil-vector-test))))

(defsystem :lil-vector-test
  :name "lil-vector-test"
  :serial t
  :description "Tests for lil-vector."
  :components
  ((:module :test
            :components ((:file "package")
                         (:file "lil-vector-test"))))
  :depends-on (:lil-vector :stefil :alexandria :check-it))
