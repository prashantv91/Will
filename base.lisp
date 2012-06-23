; Some common functions.

(defmacro with-prob (prob &rest body)
  ; Perform @body with probability @prob.
  `(if (< (random 1.0) ,prob)
     (progn ,@body)))

(defun get-last (lst)
  ; Returns last element in list @lst.
  (unless (null lst)
    (if (null (cdr lst))
      (car lst)
      (get-last (cdr lst)))))

(defun cdr-assoc (key lst)
  (unless (null lst)
    (cdr (assoc key lst))))

