; A min-heap implementation, to be used for priority queues.
; Implemented using arrays.

(in-package :will.heap)

(defstruct heap-node
  ; A node in the heap.
  key
  value)

(defclass-2 heap ()
  ; The heap structure.
  ((heap-array nil)
   (next-index 1)
   (compare-fn #'<)))

(defun array-swap (arr index1 index2)
  (let ((temp (aref arr index1)))
    (setf (aref arr index1) (aref arr index2))
    (setf (aref arr index2) temp)))

(defun heap-parent (index)
  (multiple-value-bind (quotient remainder)
    (floor index 2)
    quotient))

(defun heap-left (index)
  (* index 2))

(defun heap-right (index)
  (+ (* index 2) 1))

(defun put-in-place (heap_ index)

  (flet ((get-heap-node-key (arr ind)
           (if (>= ind (length arr))
             -1
             (let ((node (aref arr ind)))
               (if (eql node 0)
                 -1
                 (heap-node-key (aref arr ind)))))))

    (let* ((arr (heap-array heap_))
           (this (get-heap-node-key arr index))
           (parent (get-heap-node-key arr (heap-parent index)))
           (left (get-heap-node-key arr (heap-left index)))
           (right (get-heap-node-key arr (heap-right index)))
           (cmp (compare-fn heap_))
           (next (next-index heap_)))

      (flet ((do-it-next (next-index)
               (array-swap arr index next-index)
               (put-in-place heap_ next-index)))

        (if (or (= (heap-parent index) 0)
                (funcall cmp parent this))
          (if (< (heap-right index) next)
            (if (funcall cmp left right)
              (if (funcall cmp left this)
                (do-it-next (heap-left index)))
              (if (funcall cmp right this)
                (do-it-next (heap-right index))))
            (if (< (heap-left index) next)
              (if (funcall cmp left this)
                (do-it-next (heap-left index)))))
          (if (funcall cmp this parent)
            (do-it-next (heap-parent index))))))))


(defun make-heap (&optional (initial-heap-size 16) &key (cmp #'<))
  ; Create and return a heap object, with empty heap of size @initial-heap-size.
  (make-instance 'heap 
                 :heap-array (make-array initial-heap-size 
                                         :adjustable t 
                                         :element-type 'heap-node)
                 :next-index 1
                 :compare-fn cmp))

(defun heap-push (heap_ key value)
  ; Push (key, value) into heap.
  (with-slots ((arr heap-array) (next next-index)) heap_
    (if (= next (length arr))
      (adjust-array arr (* 2 (length arr))))
    (setf (aref arr next) (make-heap-node :key key :value value))
    (put-in-place heap_ next)
    (incf next)))

(defun heap-pop (heap_)
  ; Pop the top of the heap and return the value.
  (with-slots ((arr heap-array) (next next-index)) heap_
    (if (= next 1)
      nil
      (let ((retval (heap-node-value (aref arr 1))))
        (decf next)
        (setf (aref arr 1) (aref arr next))
        (put-in-place heap_ 1)
        retval))))


