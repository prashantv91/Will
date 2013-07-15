; Contains fundamental definitions added in the spur of many moments.

(in-package :will.base)

(defparameter *DEBUG* t "Determines whether debug messages are printed.")

(defun debug-print (msg &rest extra)
  (if *DEBUG*
    (if (null extra)
      (progn
        (format t "~a~%" msg)
        (finish-output nil))
      (progn
        (apply #'format t msg extra)
        (format t "~%")
        (finish-output nil)))))

(defun cd (path)
  ; Incomplete - cannot go back.
  (setf *default-pathname-defaults* (make-pathname :name path)))

(defun pwd ()
  (truename "."))

(defun vector-to-int (vect &optional (base 256))
  ; Converts a vector representation of a number in base @base to integer.
  (if (= (length vect) 0)
    0
    (+ (elt vect 0) (* base (vector-to-int (subseq vect 1))))))

(defun int-to-str (num len &optional (base 256))
  ; Converts @num to a string corresponding to ascii values in little(big?) endian format of length @length.
  (if (= len 0)
    ""
    (concatenate 'string 
                 (string (code-char (mod num base))) 
                 (int-to-str (floor num base) (- len 1)))))

(defun str-to-int (str &optional (base 256))
  ; Inverts int-to-str.
  (if (= (length str) 0)
    0
    (+ (char-code (elt str 0)) (* base (str-to-int (subseq str 1))))))



(defun expand-slots (args)
  ; Used to add keyword arguments to slot declarations by defclass-2.
  (unless (null args)
    (let ((slot-name (car (car args))) 
          (slot-val (unless (null (cdr (car args)))
                      (cadr (car args)))))
      (cons
        `(,slot-name :accessor ,slot-name 
                    :initarg ,(intern (string-upcase slot-name) :keyword)
                    :initform ,slot-val)
        (unless (null (cdr args))
          (expand-slots (cdr args)))))))

(defmacro defclass-2 (name superclasses &optional (slots nil))
  ; For easy declaration of slots in defclass. 
  ; Eg: (defclass-2 a () ((slot1 default-value) ... ))
  `(defclass ,name ,superclasses ,(expand-slots slots)))

(defun get-last (lst)
  ; Returns last element in list @lst.
  (unless (null lst)
    (if (null (cdr lst))
      (car lst)
      (get-last (cdr lst)))))

(defun cdr-assoc (key lst)
  ; Get only the data in a key-data pair (@key is key in assoc list @lst).
  (unless (null lst)
    (cdr (assoc key lst))))

(defun cdr-rassoc (key lst)
  ; Get only the data in a key-data pair (@key is key in assoc list @lst).
  (unless (null lst)
    (cdr (rassoc key lst))))

(defun load-config-file (file-name)
  ; Loads constants from the file file-name, which should be in the same directory as this.
  (with-open-file (file 
                    (make-pathname :name file-name :directory '(:relative "."))
                    :direction :input)
    (do ((pair (read file nil 'EOF)
               (read file nil 'EOF)))
      ((eql pair 'EOF))
      (progn
        (setf (symbol-value (car pair)) (cadr pair))
        (debug-print pair)))))


