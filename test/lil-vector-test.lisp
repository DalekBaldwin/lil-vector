(in-package :lil-vector-test)

(in-root-suite)

(defsuite* test-all)

(defun safe-shuffle (sequence)
  (shuffle (copy-seq sequence)))

(deftest test-static-vector ()
  (let ((*size* 1000)
        (*list-size* 1000)
        (*num-trials* 1000))
    (with-generators ((contents
                       (generator
                        (list (integer)))))
      (check-that
       (let ((conjed-pbvt (reduce #'conj-pbvt contents
                                  :initial-value (empty-pbvt)))
             (static-pbvt (apply #'static-pbvt contents)))
         (is (equal-pbvt conjed-pbvt static-pbvt)))))))

#+nil
(let ((stuff (iota 100)))
  (time
   (loop repeat 100
      collect
        ;;(reduce #'conj-pbvt stuff :initial-value (empty-pbvt))
        (apply #'static-pbvt stuff)
        ))
  nil)

(deftest vector-test ()
  (let ((*size* 1000)
        (*list-size* 1000)
        (*num-trials* 1000))
    (with-generators ((contents
                       (generator
                        (list (integer)))))
      (check-that
       (let* ((content-size (length contents))
              (indices (iota content-size))
              (pbvt (apply #'static-pbvt contents))
              (pbvt-contents (map-pbvt pbvt #'identity))
              (pbvt-contents-from-lookup
               (mapcar (lambda (x)
                         (lookup-pbvt pbvt x))
                       indices)))
         (is (equal pbvt-contents contents))
         (is (equal pbvt-contents-from-lookup contents))
         (let* ((updated-pbvt
                 (reduce (lambda (accum item)
                           (update-pbvt accum item (1+ item)))
                         (safe-shuffle indices)
                         :initial-value pbvt))
                (updated-pbvt-contents (map-pbvt updated-pbvt #'identity)))
           (is
            (equal updated-pbvt-contents
                   (mapcar #'1+ indices))))
         (or (zerop content-size)
             (let* ((to-remove (random content-size))
                    (pbvt-removed
                     (labels ((rec-pop (accum i)
                                (cond
                                  ((zerop i)
                                   accum)
                                  (t
                                   (rec-pop (pop-pbvt accum) (1- i))))))
                       (rec-pop pbvt to-remove)))
                    (pbvt-removed-contents (map-pbvt pbvt-removed #'identity)))
               (is
                (equal pbvt-removed-contents
                       (subseq contents 0 (- content-size to-remove)))))))))))

(deftest test-divide-vector ()
  (let ((*size* 1000)
        (*list-size* 1000)
        (*num-trials* 1000))
    (with-generators ((contents
                       (generator
                        (list (integer)))))
      (check-that
       (let ((pbvt (apply #'static-pbvt contents)))
         (multiple-value-bind (left right)
             (divide-pbvt pbvt)
           (is (equal contents
                      (append (collect-all left)
                              (collect-all right))))))))))
