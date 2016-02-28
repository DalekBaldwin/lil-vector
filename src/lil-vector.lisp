(in-package :lil-vector)

(define-symbol-macro _unbound_ '#:UNBOUND)

(defclass pbvt ()
  ((size
    :initarg :size)
   (node
    :initarg :node))
  (:documentation "Persistent bit-partitioned vector trie, as in Clojure.
See http://hypirion.com/musings/understanding-persistent-vector-pt-1"))

(defun empty-pbvt ()
  (make-instance
   'pbvt
   :size 0
   :node _unbound_))

(defun log-floor (number base)
  "Like `floor` with respect to logarithm instead of division.
Returns three values:
integer-valued logarithm
remainder
difference between number and next power of base (anti-remainder)"
  (cond
    ((zerop base)
     (values 0 0 0))
    ((zerop number)
     (values 0 0 0))
    ((= base 1)
     (values 0 0 0))
    (t
     (labels ((%log-floor (log cur)
                (let* ((next (* cur base))
                       (diff (- next number)))
                  (case (signum diff)
                    (-1
                     (%log-floor (1+ log) next))
                    (0
                     (values (1+ log) 0 diff))
                    (1
                     (values log (- number cur) diff))))))
       (%log-floor 0 1)))))

(let* ((bits 5) ;; make interface parametric with respect to branching factor
       (width (ash 1 bits))
       (mask (1- width)))
  (defun get-depth (size)
    (log-floor (1- size) width))
  
  (defun path-to (index size)
    (let* ((level (* bits (get-depth size))))
      (labels ((%path-to (level)
                 (cond
                   ((zerop level)
                    (list (logand index mask)))
                   (t
                    (list* (logand (ash index (- level)) mask)
                           (%path-to (- level bits)))))))
        (%path-to level))))
  
  (defun lookup-pbvt (pbvt index)
    (with-slots (node size) pbvt
      (case (signum (floor index size))
        (0
         (reduce #'aref (path-to index size) :initial-value node))
        (otherwise
         (error "bad index")))))
  
  (defun update-pbvt (pbvt index value)
    (with-slots (node size) pbvt
      (case (signum (floor index size))
        (0
         (let ((path (path-to index size)))
           (labels ((%update (path node)
                      (destructuring-bind (first-path . rest-path) path
                        (let ((new-node (copy-seq node)))
                          (cond
                            ((endp rest-path)
                             (setf (aref new-node first-path) value)
                             new-node)
                            (t
                             (setf (aref new-node first-path)
                                   (%update
                                    rest-path
                                    (aref node first-path)))
                             new-node))))))
             (make-instance
              'pbvt
              :size size
              :node (%update path node)))))
        (otherwise
         (error "bad index")))))
  
  (defun collect-all (pbvt)
    (let ((level (get-depth (slot-value pbvt 'size))))
      (labels ((%collect-all (node level)
                 (cond
                   ((eql node _unbound_)
                    nil)
                   ((zerop level)
                    (remove-if (lambda (x) (eql x _unbound_))
                               (map 'list #'identity node)))
                   (t
                    (apply #'append (map 'list
                                         (lambda (x) (%collect-all x (1- level)))
                                         node))))))
        (%collect-all (slot-value pbvt 'node) level))))
  
  (defun conj-pbvt (pbvt value)
    (with-slots (node size) pbvt
      (case size
        ;; log-floor doesn't return meaningful info for 0 or 1
        (0
         (make-instance
          'pbvt
          :size 1
          :node (make-array
                 (list width)
                 :adjustable nil
                 :initial-contents (list* value
                                          (loop repeat (1- width)
                                             collect _unbound_)))))
        (1
         (make-instance
          'pbvt
          :size 2
          :node (make-array
                 (list width)
                 :adjustable nil
                 :initial-contents (list* (aref node 0)
                                          value
                                          (loop repeat (- width 2)
                                             collect _unbound_)))))
        (otherwise
         (multiple-value-bind (log remainder) (log-floor size width)
           (cond
             ((zerop remainder) ;; root overflow case
              (labels
                  ((%conj-overflow (accum i)
                     (cond
                       ((zerop i)
                        (make-instance
                         'pbvt
                         :size (1+ size)
                         :node (make-array
                                (list width)
                                :adjustable nil
                                :initial-contents
                                (list*
                                 ;; existing top-level node in first position
                                 node
                                 ;; new node with one bottom-level entry second
                                 accum
                                 ;; then empty nodes
                                 (loop repeat (- width 2)
                                    collect _unbound_)))))
                       (t
                        (let ((new-node
                               (make-array (list width)
                                           :adjustable nil
                                           :initial-element _unbound_)))
                          (setf (aref new-node 0) accum)
                          (%conj-overflow new-node (1- i)))))))
                (%conj-overflow
                 (make-array
                  (list width)
                  :adjustable nil
                  :initial-contents (list* value
                                           (loop repeat (1- width)
                                              collect _unbound_)))
                 (1- log))))
             (t
              (let ((path (path-to size size)))
                (labels ((%conj (path node)
                           (destructuring-bind (first-path . rest-path) path
                             (cond
                               ((endp rest-path)
                                (setf (aref node first-path) value)
                                node)
                               (t
                                (let ((next-node (aref node first-path)))
                                  (setf
                                   (aref node first-path)
                                   (%conj
                                    rest-path
                                    (cond
                                      ((eql next-node _unbound_)
                                       (make-array
                                        (list width)
                                        :adjustable nil
                                        :initial-element _unbound_))
                                      (t
                                       (copy-seq next-node))))))
                                node)))))
                  (make-instance
                   'pbvt
                   :size (1+ size)
                   :node (%conj path (copy-seq node))))))))))))
  
  (defun pop-pbvt (pbvt)
    (with-slots (node size) pbvt
      (case size
        (0
         (error "attempted to pop empty vector"))
        (1
         (values (empty-pbvt) (aref node 0)))
        ;; don't take log-floor of 1
        (2
         (values
          (make-instance
           'pbvt
           :size 1
           :node (make-array
                  (list width)
                  :adjustable nil
                  :initial-contents
                  (list* (aref node 0)
                         (loop repeat (1- width)
                            collect _unbound_))))
          (aref node 1)))
        (otherwise
         (multiple-value-bind (log remainder) (log-floor (1- size) width)
           (cond
             ((zerop remainder) ;; root killing case
              (values
               (make-instance
                'pbvt
                :size (1- size)
                :node (aref node 0))
               (reduce #'aref
                       (list* 1 (loop repeat log collect 0))
                       :initial-value node)))
             (t
              (let ((path (path-to (1- size) size))
                    (new-path (path-to (- size 2) size)))
                (labels ((%pop (path new-path node)
                           (destructuring-bind
                                 (first-path . rest-path) path
                             (destructuring-bind
                                   (first-new-path . rest-new-path) new-path
                               (cond
                                 ((endp rest-path)
                                  (setf (aref node first-path) _unbound_)
                                  node)
                                 ((= first-path first-new-path)
                                  (setf
                                   (aref node first-path)
                                   (%pop
                                    rest-path
                                    rest-new-path
                                    (copy-seq (aref node first-path))))
                                  node)
                                 (t
                                  (setf (aref node first-path) _unbound_)
                                  node))))))
                  (values
                   (make-instance
                    'pbvt
                    :size (1- size)
                    :node (%pop path new-path (copy-seq node)))
                   (lookup-pbvt pbvt (1- size)))))))))))))

;; print vector clojurely
(defmethod print-object ((object pbvt) stream)
  (let ((elems (collect-all object)))
    (pprint-logical-block
        (stream elems :prefix "[" :suffix "]")
      (pprint-indent :current 1)
      (pprint-exit-if-list-exhausted)
      (loop
         (write (pprint-pop) :stream stream)
         (pprint-exit-if-list-exhausted)
         (write-char #\Space stream)
         (pprint-newline :fill stream)))))