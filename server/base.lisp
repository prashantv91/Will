; Contains fundamental definitions added in the spur of many moments.

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



(defun load-config-file (file-name)
  ; Loads constants from the file file-name, which should be in the same directory as this.
  (with-open-file (file 
                    (make-pathname :name file-name :directory '(:relative "."))
                    :direction :input)
    (do ((pair (read file nil 'EOF)
               (read file nil 'EOF)))
      ((eql pair 'EOF))(progn
      (setf (symbol-value (car pair)) (cadr pair))
      (print pair)))))



