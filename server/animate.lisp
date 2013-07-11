; Class declarations, etc. for animate objects.
; Currently only place-holders, as this is, for now, only for client handling.

(in-package :will.animate)

(defparameter *species* nil)   ; Stores the species, the leaves of the tree of monster classes.

(defmacro defmonsterclass (&rest args)
  ; To define a class broader than a species.
  (cons 'defclass-2 args))

(defmacro defmonster (&rest args)
  ; To define a species.
  (progn 
    (setf *species* (cons (car args) *species*))
    (cons 'defclass-2 args)))

(defmonsterclass monster ()
                 ((brain nil)
                  (backup-brain nil)
                  (possessed nil)
                  (lock (make-mutex))))

(defmonsterclass canine (monster)
                 ((forelimb 2)
                  (hindlimb 2)
                  (head 1)))

(defmonster dog (canine)
            ((size 20)))

                    


