(in-package :lil-vector)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-symbol-macro +unbound+ (load-time-value __unbound__)))
(defvar __unbound__ '#:UNBOUND)

(defstruct pbvt
  "Persistent bit-partitioned vector trie, as in Clojure.
See http://hypirion.com/musings/understanding-persistent-vector-pt-1"
  (size
   0
   :type fixnum
   :read-only t)
  (node
   #()
   :type simple-vector
   :read-only t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-symbol-macro +empty-pbvt+ (load-time-value __empty-pbvt__)))
(defvar __empty-pbvt__
  (make-pbvt
   :size 0
   :node (make-array 0)))

(defun empty-pbvt ()
  +empty-pbvt+)

(defun log-floor (number base)
  "Like `floor` with respect to logarithm instead of division.
Returns three values:
integer-valued logarithm
remainder
difference between number and next power of base (anti-remainder)"
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
    (%log-floor 0 1)))

(defmacro let-accessed (slots instance &body body)
  "Bind variables via accessors one time only at the top of the scope."
  (once-only (instance)
    `(let (,@(loop for slot-entry in slots
                collect
                  (cond
                    ((listp slot-entry)
                     (destructuring-bind
                           (var-name accessor-name) slot-entry
                       `(,var-name (,accessor-name ,instance))))
                    (t
                     `(,slot-entry (,slot-entry ,instance))))))
       ,@body)))

(defun lookup-pbvt (pbvt index)
  (let-accessed ((node pbvt-node)
                 (size pbvt-size)) pbvt
    (case (signum (floor index size))
      (0
       (reduce #'aref (path-to index size) :initial-value node))
      (otherwise
       (error "bad index")))))

(defun update-pbvt (pbvt index value)
  (let-accessed ((node pbvt-node)
                 (size pbvt-size)) pbvt
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
           (make-pbvt
            :size size
            :node (%update path node)))))
      (otherwise
       (error "bad index")))))

(defun map-pbvt (pbvt fun)
  (let-accessed ((node pbvt-node)
                 (size pbvt-size)) pbvt
    (cond
      ((zerop size)
       nil)
      (t
       (let ((level (get-depth size)))
         (labels ((%map (level node)
                    (cond
                      ((zerop level)
                       (loop for val across node
                          until (eql val +unbound+)
                          collect (funcall fun val)))
                      (t
                       (loop for next-node across node
                          until (eql next-node +unbound+)
                          append (%map (1- level) next-node))))))
           (%map level node)))))))

(defun fold-left-pbvt (pbvt function seed)
  (cond
    ((eql pbvt +empty-pbvt+)
     seed)
    (t
     (let-accessed ((node pbvt-node)
                    (size pbvt-size)) pbvt
       ;;#+nil
       (let ((level (get-depth size))
             (path (path-to (1- size) size)))
         (labels ((%fold-all (level node seed)
                    (cond
                      ((zerop level)
                       (reduce function node :initial-value seed))
                      (t
                       (reduce (lambda (accum item)
                                 (%fold-all (1- level) item accum))
                               node
                               :initial-value seed))))
                  (%fold-left (level path node seed)
                    (cond
                      ((zerop level)
                       (reduce function node
                               :end (1+ (first path))
                               :initial-value seed))
                      (t
                       (destructuring-bind (first-path . rest-path) path
                         (let ((full-subtree-accum
                                (reduce (lambda (accum item)
                                          (%fold-all (1- level) item accum))
                                        node
                                        :end first-path
                                        :initial-value seed)))
                           (%fold-left
                            (1- level)
                            rest-path
                            (aref node first-path)
                            full-subtree-accum)))))))
           (%fold-left level path node seed)))
       #+nil
       (let ((level (get-depth size)))
         (labels ((%fold-left (level node seed)
                    (cond
                      ((zerop level)
                       (catch 'unbound
                         (reduce (lambda (accum item)
                                   (cond
                                     ((eql item +unbound+)
                                      (throw 'unbound accum))
                                     (t
                                      (funcall function accum item))))
                                 node
                                 :initial-value seed)))
                      (t
                       (catch 'unbound
                         (reduce (lambda (accum item)
                                   (cond
                                     ((eql item +unbound+)
                                      (throw 'unbound accum))
                                     (t
                                      (%fold-left (1- level) item accum))))
                                 node
                                 :initial-value seed))))))
           (%fold-left level node seed)))))))

(defun fold-right-pbvt (pbvt function seed)
  (cond
    ((eql pbvt +empty-pbvt+)
     seed)
    (t
     (let-accessed ((node pbvt-node)
                    (size pbvt-size)) pbvt
       (let ((level (get-depth size))
             (path (path-to (1- size) size)))
         (labels ((%fold-all (level node seed)
                    (cond
                      ((zerop level)
                       (reduce function node
                               :from-end t
                               :initial-value seed))
                      (t
                       (reduce (lambda (item accum)
                                 (%fold-all (1- level) item accum))
                               node
                               :from-end t
                               :initial-value seed))))
                  (%fold-right (level path node seed)
                    (cond
                      ((zerop level)
                       (reduce function node
                               :from-end t
                               :end (1+ (first path))
                               :initial-value seed))
                      (t
                       (destructuring-bind (first-path . rest-path) path
                         (let ((partial-subtree-accum
                                (%fold-right
                                 (1- level)
                                 rest-path
                                 (aref node first-path)
                                 seed)))
                           (reduce (lambda (item accum)
                                     (%fold-all (1- level) item accum))
                                   node
                                   :from-end t
                                   :end first-path
                                   :initial-value partial-subtree-accum)))))))
           (%fold-right level path node seed)))))))

(defun collect-all (pbvt)
  (map-pbvt pbvt #'identity))

(defun partial-update-array-from-list (array start end source)
  (let ((new-array (copy-seq array)))
    (values
     (loop for i from start below end
        for things on source
        do (setf (aref new-array i) (first things))
        finally (return things))
     new-array)))

(let* ((bits 5) ;; make interface parametric with respect to branching factor
       (width (ash 1 bits))
       (width-1 (1- width))
       (width-2 (- width 2))
       (mask (1- width)))
  (defun get-depth (size)
    (log-floor (1- size) width))

  (defun path-to (index size)
    (let ((level (* bits (get-depth size))))
      (labels ((%path-to (accum level)
                 (cond
                   ((zerop level)
                    (nreverse (list* (logand index mask) accum)))
                   (t
                    (%path-to (list* (logand (ash index level) mask) accum)
                              (+ level bits))))))
        (%path-to nil (- level)))))

  (defun static-pbvt (&rest arguments)
    "When creating a vector from a known list of contents, we can allocate
exactly the memory we require instead of recursively conj-ing and throwing away
partially-filled arrays."
    (cond
      ((endp arguments)
       (empty-pbvt))
      (t
       (let ((size (length arguments)))
         (labels
             ((%static (stuff size)
                (let* ((array-chunks
                        (loop for chunk on stuff
                           by (lambda (x) (nthcdr width x))
                           for remaining = size then (- remaining width)
                           collect
                             (make-array
                              width
                              :initial-contents
                              (cond
                                ((<= remaining width)
                                 (append (subseq chunk 0 remaining)
                                         (loop repeat (- width remaining)
                                            collect +unbound+)))
                                (t (subseq chunk 0 width)))))))
                  (cond
                    ((<= size width)
                     (first array-chunks))
                    (t
                     (%static array-chunks (length array-chunks)))))))
           (make-pbvt
            :size size
            :node (%static arguments size)))))))

  (defun conj-pbvt (pbvt value)
    (let-accessed ((node pbvt-node) (size pbvt-size)) pbvt
      (case size
        ;; log-floor doesn't return meaningful info for 0 or 1
        (0
         (make-pbvt
          :size 1
          :node (make-array
                 width
                 :adjustable nil
                 :initial-contents (list* value
                                          (loop repeat width-1
                                             collect +unbound+)))))
        (1
         (make-pbvt
          :size 2
          :node (make-array
                 width
                 :adjustable nil
                 :initial-contents (list* (aref node 0)
                                          value
                                          (loop repeat width-2
                                             collect +unbound+)))))
        (otherwise
         (multiple-value-bind (log remainder) (log-floor size width)
           (cond
             ((zerop remainder) ;; root overflow case
              (labels
                  ((%conj-overflow (accum i)
                     (cond
                       ((zerop i)
                        (make-pbvt
                         :size (1+ size)
                         :node (make-array
                                width
                                :adjustable nil
                                :initial-contents
                                (list*
                                 ;; existing top-level node in first position
                                 node
                                 ;; new node with one bottom-level entry second
                                 accum
                                 ;; then empty nodes
                                 (loop repeat width-2
                                    collect +unbound+)))))
                       (t
                        (let ((new-node
                               (make-array width
                                           :adjustable nil
                                           :initial-element +unbound+)))
                          (setf (aref new-node 0) accum)
                          (%conj-overflow new-node (1- i)))))))
                (%conj-overflow
                 (make-array
                  width
                  :adjustable nil
                  :initial-contents (list* value
                                           (loop repeat width-1
                                              collect +unbound+)))
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
                                      ((eql next-node +unbound+)
                                       (make-array
                                        width
                                        :adjustable nil
                                        :initial-element +unbound+))
                                      (t
                                       (copy-seq next-node))))))
                                node)))))
                  (make-pbvt
                   :size (1+ size)
                   :node (%conj path (copy-seq node))))))))))))

  (defun pop-pbvt (pbvt)
    (let-accessed ((node pbvt-node) (size pbvt-size)) pbvt
      (case size
        (0
         (error "attempted to pop empty vector"))
        (1
         (values (empty-pbvt) (aref node 0)))
        ;; don't take log-floor of 1
        (2
         (values
          (make-pbvt
           :size 1
           :node (make-array
                  width
                  :adjustable nil
                  :initial-contents
                  (list* (aref node 0)
                         (loop repeat width-1
                            collect +unbound+))))
          (aref node 1)))
        (otherwise
         (multiple-value-bind (log remainder) (log-floor (1- size) width)
           (cond
             ((zerop remainder) ;; root killing case
              (values
               (make-pbvt
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
                                  (setf (aref node first-path) +unbound+)
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
                                  (setf (aref node first-path) +unbound+)
                                  node))))))
                  (values
                   (make-pbvt
                    :size (1- size)
                    :node (%pop path new-path (copy-seq node)))
                   (lookup-pbvt pbvt (1- size))))))))))))

  (defun divide-pbvt (pbvt)
    ;; a lot of special cases here. (width-1)/width of the time, we need to copy
    ;; a lot of stuff to create a new right vector out of elements that are not
    ;; aligned at the same offset, so we only divide things up at the first or
    ;; second level to make this operation effectively connstant time while,
    ;; making the resulting vectors comparably sized, especially with a
    ;; bit-basis of 5 like Clojure has
    (let-accessed ((node pbvt-node) (size pbvt-size)) pbvt
      (case size
        (0
         (values +empty-pbvt+ +empty-pbvt+))
        (1
         (values +empty-pbvt+ pbvt))
        (otherwise
         (let ((level (get-depth size)))
           (case level
             (0
              (let ((left-size (floor size 2)))
                (values
                 (make-pbvt
                  :size left-size
                  :node (let ((array (copy-seq node)))
                          (loop for i from left-size below width
                             do (setf (aref array i) +unbound+))
                          array))
                 (make-pbvt
                  :size (- size left-size)
                  :node (make-array
                         width
                         :initial-contents
                         (append
                          (loop for i from left-size below width
                             collect (aref node i))
                          (loop repeat left-size collect +unbound+)))))))
             (otherwise
              (destructuring-bind (first-path second-path . rest-path)
                  (path-to (1- size) size)
                (declare (ignore rest-path))
                (let* ((chunk-size (expt width (1- level)))
                       (num-chunks
                        (+ (* first-path width)
                           (1+ second-path)))
                       (left-chunks (floor num-chunks 2))
                       (left-size (* chunk-size left-chunks))
                       (right-size (- size left-size)))
                  (case first-path
                    (1 ;; resulting vectors will be one level lower
                     (let ((node0 (aref node 0))
                           (node1 (aref node 1)))
                       (values
                        (make-pbvt
                         :size left-size
                         :node (let* ((array (copy-seq node0)))
                                 (loop for i from left-chunks below width
                                    do (setf (aref array i) +unbound+))
                                 array))
                        (make-pbvt
                         :size right-size
                         :node (make-array
                                width
                                :initial-contents
                                (append (loop for i from left-chunks below width
                                           collect (aref node0 i))
                                        (loop for i from 0 upto second-path
                                           collect (aref node1 i))
                                        (loop for i from 0
                                           below (- (+ width left-chunks)
                                                    num-chunks)
                                           collect +unbound+)))))))
                    (otherwise
                     (multiple-value-bind (outer-offset inner-offset)
                         (floor left-chunks width)
                       (let* ((splitting-node (aref node outer-offset)))
                         (values
                          (make-pbvt
                           :size left-size
                           :node
                           (cond
                             ((and (= outer-offset 1)
                                   (= inner-offset 0))
                              (aref node 0))
                             (t
                              (make-array
                               width
                               :initial-contents
                               (append
                                (loop for i from 0 below outer-offset
                                   for subnode across node
                                   collect subnode)
                                (list
                                 (let ((array (copy-seq splitting-node)))
                                   (loop for i from inner-offset below width
                                      do (setf (aref array i) +unbound+))
                                   array))
                                (loop for i from (1+ outer-offset)
                                   below width
                                   collect +unbound+))))))
                          (make-pbvt
                           :size right-size
                           :node
                           (case inner-offset
                             (0
                              (coerce
                               (loop for i from outer-offset upto first-path
                                  collect (aref node i))
                               'vector))
                             (otherwise
                              (coerce
                               (append
                                (loop for i from outer-offset below first-path
                                   collect
                                     (make-array
                                      width
                                      :initial-contents
                                      (let ((subnode (aref node i))
                                            (next-subnode (aref node (1+ i))))
                                        (append
                                         (loop for j from inner-offset below width
                                            collect (aref subnode j))
                                         (loop for j from 0 below inner-offset
                                            collect (aref next-subnode j))))))
                                (let ((subnode (aref node first-path)))
                                  (list
                                   (make-array
                                    width
                                    :initial-contents
                                    (append
                                     (loop for i from inner-offset below width
                                        collect (aref subnode i))
                                     (loop repeat inner-offset
                                        collect +unbound+)))))
                                (loop repeat (- width first-path)
                                   collect +unbound+))
                               'vector))))))))))))))))))

  (defun fold-left*-pbvt (pbvt function seed)
    (cond
      ((eql pbvt +empty-pbvt+)
       seed)
      (t
       (let-accessed ((node pbvt-node) (size pbvt-size)) pbvt
         (let ((level (get-depth size))
               (path (path-to (1- size) size)))
           (labels
               ((%fold-all (level node seed)
                  (cond
                    ((zerop level)
                     (reduce (lambda (accum item)
                               (destructuring-bind (count . stuff)
                                   accum
                                 (cons
                                  (1+ count)
                                  (funcall function stuff count item))))
                             node
                             :initial-value seed))
                    (t
                     (reduce (lambda (accum item)
                               (%fold-all (1- level) item accum))
                             node
                             :initial-value seed))))
                (%fold-left* (level path node seed)
                  (cond
                    ((zerop level)
                     (reduce (lambda (accum item)
                               (destructuring-bind (count . stuff)
                                   accum
                                 (cons
                                  (1+ count)
                                  (funcall function stuff count item))))
                             node
                             :end (1+ (first path))
                             :initial-value seed))
                    (t
                     (destructuring-bind (first-path . rest-path) path
                       (let ((full-subtree-accum
                              (reduce (lambda (accum item)
                                        (%fold-all (1- level) item accum))
                                      node
                                      :end first-path
                                      :initial-value seed)))
                         (%fold-left*
                          (1- level)
                          rest-path
                          (aref node first-path)
                          full-subtree-accum)))))))
             (cdr
              (%fold-left* level path node (cons 0 seed)))))
         #+nil
         (let* ((level (get-depth size)))
           (labels
               ((%fold-left* (level node seed)
                  (cond
                    ((zerop level)
                     (catch 'unbound
                       (reduce (lambda (accum item)
                                 (cond
                                   ((eql item +unbound+)
                                    (throw 'unbound accum))
                                   (t
                                    (destructuring-bind
                                          (count . stuff)
                                        accum
                                      (cons
                                       (1+ count)
                                       (funcall function
                                                stuff count item))))))
                               node
                               :initial-value seed)))
                    (t
                     (catch 'unbound
                       (reduce (lambda (accum item)
                                 (cond
                                   ((eql item +unbound+)
                                    (throw 'unbound accum))
                                   (t
                                    (%fold-left* (1- level) item accum))))
                               node
                               :initial-value seed))))))
             (cdr
              (%fold-left* level node (cons 0 seed)))))))))

  (defun fold-right*-pbvt (pbvt function seed)
    (cond
      ((eql pbvt +empty-pbvt+)
       seed)
      (t
       (let-accessed ((node pbvt-node) (size pbvt-size)) pbvt
         (let ((level (get-depth size))
               (path (path-to (1- size) size)))
           (labels ((%fold-all (level node seed)
                      (cond
                        ((zerop level)
                         (reduce (lambda (item accum)
                                   (destructuring-bind (index . stuff)
                                       accum
                                     (cons
                                      (1- index)
                                      (funcall function index item stuff))))
                                 node
                                 :from-end t
                                 :initial-value seed))
                        (t
                         (reduce (lambda (accum item)
                                   (%fold-all (1- level) item accum))
                                 node
                                 :from-end t
                                 :initial-value seed))))
                    (%fold-right (level path node seed)
                      (cond
                        ((zerop level)
                         (reduce (lambda (item accum)
                                   (destructuring-bind (index . stuff)
                                       accum
                                     (cons
                                      (1- index)
                                      (funcall function index item stuff))))
                                 node
                                 :from-end t
                                 :end (1+ (first path))
                                 :initial-value seed))
                        (t
                         (destructuring-bind (first-path . rest-path) path
                           (let ((partial-subtree-accum
                                  (%fold-right
                                   (1- level)
                                   rest-path
                                   (aref node first-path)
                                   seed)))
                             (reduce (lambda (item accum)
                                       (%fold-all (1- level) item accum))
                                     node
                                     :from-end t
                                     :end first-path
                                     :initial-value partial-subtree-accum)))))))
             (cdr
              (%fold-right level path node (cons (1- size) seed))))))))))

(defun equal-pbvt (x y)
  (let-accessed ((size-x pbvt-size) (node-x pbvt-node)) x
    (let-accessed ((size-y pbvt-size) (node-y pbvt-node)) y
      (and (= size-x size-y)
           (equalp node-x node-y)))))

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

(interface:define-interface <vector> (pure:<map>)
  ()
  (:abstract)
  (:generic> slice (vector start end))
  (:generic> insert-at (vector pos value))
  (:generic> splice-at (vector pos vector))
  (:generic> concat (vector1 vector2)))

(interface:define-interface <pbvt>
    (pure:<copy-is-identity>
     ;;pure:<map-foldable-from-*>
     ;;pure:<map-fold-right*-from-fold-left*>
     ;;pure:<map-has-key-p-from-lookup>
     ;;pure:<map-join-from-fold-left*-insert>
     ;;pure:<map-singleton-from-insert>
     ;;pure:<map-update-key-from-lookup-insert-drop>
     <vector>)
  ()
  (:method> pure:empty ()
    +empty-pbvt+)
  (:method> pure:lookup (map key)
    (lookup-pbvt map key))
  ;;(:method> pure:insert (map key value))
  ;;(:method> pure:drop (map key))
  (:method> pure:fold-left (foldable function seed)
    (fold-left-pbvt foldable function seed))
  (:method> pure:fold-left* (foldable function seed)
    (fold-left*-pbvt foldable function seed))
  (:method> pure:fold-right (map function seed)
    (fold-right-pbvt foldable function seed))
  (:method> pure:fold-right* (map function seed)
    (fold-right*-pbvt foldable function seed))
  ;;(:method> pure:first-key-value (map))
  (:method> pure:divide (collection)
    (divide-pbvt collection))
  (:singleton)
  (:documentation "persistent bit-partitioned vector trie"))
