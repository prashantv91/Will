; Map generator functions.
; Each of these must take height, width, maybe some more arguments and return appropriate map array.

(in-package :will.map)

(defmacro for-tiles-in (map-array tile-var yargs xargs &rest body)
  ; Loops through all elements of @map-array in the range specified 
  ; by yargs and xargs.
  ; (for-tiles-in array-name tile-var-name 
  ;               (ybegin yend) (xbegin xend)
  ;               body)
  (let ((yvar (gensym)) (xvar (gensym)))
   `(loop
      for ,yvar from ,(car yargs) to ,(cadr yargs)
      do
      (loop
        for ,xvar from ,(car xargs) to ,(cadr xargs)
        do
        (let ((,tile-var (aref ,map-array ,yvar ,xvar)))
          ,@body
          )))))

(defmacro for-coords-in (yvar yargs xvar xargs &rest body)
  ; Loops through coordinates in the range specified by yargs and xargs.
  ; Temporary. Find a way to make @tile-var setfable. 
  ; Then this macro is unnecessary.
  ; (for-coords-in yvar (ybegin yend) 
  ;                xvar (xbegin xend)
  ;               body)
  `(loop
     for ,yvar from ,(car yargs) to ,(cadr yargs)
     do
     (loop
       for ,xvar from ,(car xargs) to ,(cadr xargs)
       do
       ,@body)))


(defun empty-map (height width)
  (let ((map-array (make-array (list height width))))
    (for-coords-in y (0 (- height 1))
                   x (0 (- width 1))
      (setf (aref map-array y x) (make-tile 'grass)))
                  
    map-array))


