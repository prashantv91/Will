; Class declarations, etc. for animate objects.
; Currently only place-holders, as this is, for now, only for client handling.

(in-package :will.animate)

(defparameter *monsters* nil)   ; Stores the species, the leaves of the tree of monster classes.

(defmacro defmonsterclass (&rest args)
  (eval (cons 'defclass-2 args)))

(defmacro defmonster (&rest args)
  (progn 
    (setf *monsters* (cons (car args) *monsters*))
    (eval (cons 'defclass-2 args))))

(defmonsterclass monster ()
                 ((brain nil)))

(defmonsterclass canine (monster)
                 ((forelimb 2)
                  (hindlimb 2)
                  (head 1)))

(defmonster dog (canine)
            ((size 20)))

                    


