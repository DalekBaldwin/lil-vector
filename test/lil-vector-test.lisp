(in-package :lil-vector-test)

(in-root-suite)

(defsuite* test-all)

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
              (pbvt
               (reduce (lambda (accum item)
                         (conj-pbvt accum item))
                       contents
                       :initial-value (empty-pbvt)))
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
                         (shuffle (copy-seq indices))
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
