; Class declarations for animate objects.

(load "rules.lisp")

(defclass animate ()
  ((health :accessor health :initarg :health)
   (size :accessor size :initarg :size)))



