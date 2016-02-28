(in-package :lil-vector-test)

(in-root-suite)

(defsuite* test-all)

#+nil
(let* ((max 100)
       (thing
        (reduce (lambda (accum item)
                  (list* (conj-pbvt (first accum) item) accum))
                (iota (1- max) :start 1)
                :initial-value (list (conj-pbvt (empty-pbvt) 0)))))
  (let ((reduced
         (reduce (lambda (accum item)
                   (update-pbvt accum item
                                (* 2 (lookup-pbvt accum item))))
                 (iota max)
                 :initial-value (first thing))))
    (labels ((rec (accum i)
               (cond
                 ((zerop i)
                  (pop-pbvt accum))
                 (t
                  (rec (pop-pbvt accum) (1- i))))))
      (print
       (list thing reduced (rec reduced 99))))))
