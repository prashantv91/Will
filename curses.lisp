
(in-package :cl-user)

(defpackage :curses
  (:use :common-lisp :cffi :trivial-gray-streams)
  (:export :refresh :erase :move :*stdscr*
           :create-console :close-console :connect-console
           :echo :noecho
           :addch :mvaddch :addstr :mvaddstr
           :printw :mvprintw :attron :Attroff
           :attrset :getch :mvgetch :getyx :attrset
           :getstr :mvgetstr :getnstr :mvgetnstr
           :scrollok :scroll :scrl
           :derwin :newwin :subwin :delwin
           :wrefresh :werase :waddch :mvwaddch
           :wgetch :mvwgetch :wgetstr :mvwgetstr
           :wattrset :waddstr :mvwaddstr :wgetyx
           :ungetch :wechochar
           :curses-code-char :repl-console
           ))

(in-package :curses)

(define-foreign-library curses
  (:unix "libncurses.so.5")
  (t (:default "curses")))

;(eval-when (:compile-toplevel :load-toplevel :execute)

(use-foreign-library curses);)

(defctype chtype :long)

(defcfun "initscr" :pointer)

(defcfun "noecho" :int)

(defcfun "echo" :int)

(defcfun "nonl" :int)

(defcfun "refresh" :int)

(defcfun "erase" :int)

(defcfun "keypad" :int (wnd :pointer) (bool :boolean))

(defcvar "stdscr" :pointer)

(defcfun "cbreak" :int)

(defcfun "nocbreak" :int)

(defcfun "start_color" :int)

(defcfun "intrflush" :int (wnd :pointer) (bool :boolean))

(defcfun "endwin" :int)

(defcfun "nodelay" :int (wnd :pointer) (bool :boolean))


(defcfun "init_pair" :int (pair :short) 
         (foreground :short) (background :short))


;;Default colors

#-unix
(progn
  (defconstant +color-black+ 0)
  (defconstant +color-blue+ #x0001)
  (defconstant +color-green+ #x0002)
  (defconstant +color-red+ #x0004)
  (defconstant +color-cyan+ (logior +color-blue+ +color-green+))
  (defconstant +color-magenta+ (logior +color-red+ +color-blue+))
  (defconstant +color-yellow+ (logior +color-red+ +color-green+))
  (defconstant +color-white+ (logior +color-red+ +color-blue+ +color-green+))
  (defconstant +a-bold+ #x00800000))

#+unix
(progn
  (defconstant +color-black+ 0)
  (defconstant +color-blue+ #x0004)
  (defconstant +color-green+ #x0002)
  (defconstant +color-red+ #x0001)
  (defconstant +color-cyan+ (logior +color-blue+ +color-green+))
  (defconstant +color-magenta+ (logior +color-red+ +color-blue+))
  (defconstant +color-yellow+ (logior +color-red+ +color-green+))
  (defconstant +color-white+ (logior +color-red+ +color-blue+ +color-green+))
  (defconstant +a-bold+ #x00200000))

(defun init-colors ()
  (init-pair +color-black+ +color-black+ +color-black+)
  (init-pair +color-blue+ +color-blue+ +color-black+)
  (init-pair +color-green+ +color-green+ +color-black+)
  (init-pair +color-red+ +color-red+ +color-black+)
  (init-pair +color-cyan+ +color-cyan+ +color-black+)
  (init-pair +color-magenta+ +color-magenta+ +color-black+)
  (init-pair +color-yellow+ +color-yellow+ +color-black+)
  (init-pair +color-white+ +color-white+ +color-black+))

;;Compatibility layer for old socket interface
(defun connect-console ()
  (use-foreign-library curses)
  (initscr)
  (keypad *stdscr* t)
  (nonl) (cbreak) (noecho)
  (intrflush *stdscr* nil)
  (start-color)
  (init-colors))

(defun create-console (exe)
  (declare (ignore exe))
  (connect-console))

(defun close-console () (endwin))

;;Functions that do something interesting

(defcfun "move" :int (y :int) (x :int))

(defcfun ("addch" c-addch) :int (char chtype))

(defun addch (char)
  (c-addch (char-code char)))

(defcfun ("mvaddch" c-mvaddch) :int (y :int) (x :int) (char chtype))

(defun mvaddch (y x char)
  (c-mvaddch y x (char-code char)))

(defcfun "addstr" :int (string :string))

(defcfun "mvaddstr" :int (y :int) (x :int) (string :string))

;;The next two functions are just synonyms for the two above. If you want
;;control strings, use format.

(defun printw (str) (addstr str))

(defun mvprintw (y x str) (mvaddstr y x str))

;;Attributes conversion

;(defconstant +a-attributes+ #xffff0000L)

(defconstant +a-attributes+ #xffffff00)

#+unix(defcfun ("COLOR_PAIR" color-pair) :int (color :int))

#-unix
(defun color-pair (n)
  (ash n 24))

(defun color-code (attr)
  (case attr
    (:cblack (color-pair +color-black+))
    (:cblue (color-pair +color-blue+))
    (:CDGREEN (color-pair +color-GREEN+))
    (:CCYAN (color-pair +color-CYAN+))
    (:CRED (color-pair +color-RED+))
    (:CPURPLE (color-pair +color-MAGENTA+))
    (:CBROWN (color-pair +color-YELLOW+))
    (:CGRAY (color-pair +color-WHITE+))
    (:CDARK (+ (color-pair +color-BLACK+) +a-bold+))
    (:CLBLUE (+ (color-pair +color-BLUE+) +a-bold+))
    (:CGREEN (+ (color-pair +color-GREEN+) +a-bold+))
    (:CSKY (+ (color-pair +color-CYAN+) +a-bold+))
    (:CROSE (+ (color-pair +color-RED+) +a-bold+))
    (:CPINK (+ (color-pair +color-MAGENTA+) +a-bold+))
    (:CYELLOW (+ (color-pair +color-YELLOW+) +a-bold+))
    (:CWHITE (+ (color-pair +color-WHITE+) +a-bold+))
    (t 0)))

(defcfun ("attron" c-attron) :int (attr :int))

(defcfun ("attroff" c-attroff) :int (attr :int))



(defun attron (attr)
  (c-attron (color-code attr)))

(defun attroff (attr)
  (c-attroff (color-code attr)))

;;curses.dll doesn't have attrset for some reason
(defcfun ("wattrset" c-attrset) :int (window :pointer) (attr :int))

(defun attrset (attr)
  (c-attrset *stdscr* (color-code attr)))


;(defcfun ("getch" c-getch) :int)

(defcfun "wgetch" :int (window :pointer))

(defun getch ()
  (refresh)
  (let ((c (wgetch *stdscr*)))
    (mvprintw 24 1 (format nil "[~d]" c)) (refresh)
    c))

(defcfun ("mvgetch" c-mvgetch) :int (y :int) (x :int))

(defun mvgetch (y x)
  (let ((c (c-mvgetch y x)))
    (mvprintw 24 1 (format nil "[~d]" c)) (refresh)
    c))

(defcstruct window
  (cury :int)
  (curx :int)
  (maxy :int)
  (maxx :int))

(defun getyx ()
  (with-foreign-slots ((cury curx) *stdscr* window)
    (list cury curx)))

(defun wgetyx (wnd)
  (with-foreign-slots ((cury curx) wnd window)
    (list cury curx)))

;;Getstr

(defcfun ("getstr" c-getstr) :int (str :pointer))

(defun getstr ()
  (prog2
      (echo)
      (with-foreign-pointer-as-string (str 80)
        (c-getstr str))
    (noecho)))

(defcfun ("mvgetstr" c-mvgetstr) :int (y :int) (x :int) (str :pointer))

(defun mvgetstr (y x)
  (prog2
      (echo)
      (with-foreign-pointer-as-string (str 80)
        (c-mvgetstr y x str))
    (noecho)))

;;curses.dll doesn't have getnstr and mvgetnstr for some reason

(defcfun ("wgetnstr" c-getnstr) :int (window :pointer) (str :pointer) (n :int))

(defun getnstr (n) 
  (prog2
      (echo)
      (with-foreign-pointer-as-string (str 80);n)
        (c-getnstr *stdscr* str n))
    (noecho)))

;(defcfun ("wmvgetstr" c-mvgetnstr) :int (window :pointer) (y :int) (x :int) 
;         (str :pointer) (n :int))

(defun mvgetnstr (y x n) 
  (move y x)
  (prog2
      (echo)
      (with-foreign-pointer-as-string (str 80);n)
        (c-getnstr *stdscr* str n))
    (noecho)))

;;Scrolling

(defcfun "scrollok" :int (window :pointer) (bf :boolean))

(defcfun "scroll" :int (window :pointer))

(defcfun "scrl" :int (n :int))

;;Windows

(defcfun "derwin" :pointer (orig :pointer) 
         (nlines :int) (ncols :int) (begin-y :int) (begin-x :int))

(defcfun "newwin" :pointer (nlines :int) (ncols :int) 
         (begin-y :int) (begin-x :int))

(defcfun "subwin" :pointer (orig :pointer) 
         (nlines :int) (ncols :int) (begin-y :int) (begin-x :int))

(defcfun "delwin" :int (window :pointer))

(defcfun "wrefresh" :int (window :pointer))

(defcfun "werase" :int (window :pointer))

(defcfun ("waddch" c-waddch) :int (window :pointer) (char chtype))

(defun waddch (window char) (c-waddch window (char-code char)))

(defcfun "wmove" :int (window :pointer) (y :int) (x :int))

(defun mvwaddch (w y x c) (wmove w y x) (waddch w c))

(defcfun "mvwgetch" :int (window :pointer) (y :int) (x :int))


(defcfun ("wgetstr" c-wgetstr) :int (window :pointer) (str :pointer))

(defun wgetstr (window)
  (prog2
      (echo)
      (with-foreign-pointer-as-string (str 80)
        (c-wgetstr window str))
    (noecho)))

(defun mvwgetstr (window y x)
  (wmove window y x)
  (wgetstr window))

(defun wattrset (window attr)
  (c-attrset window (color-code attr)))

(defcfun "waddstr" :int (window :pointer) (string :string))

(defcfun "mvwaddstr" :int (window :pointer) (y :int) (x :int) (string :string))


(defun curses-code-char (code)
  "Returns a character that is corresponding to a given curses
code. May be system dependent."
  (case code
    ;Numpad
    (458 #\/) (463 #\*) (464 #\-) (465 #\+) (459 #\Return)
    (t (if (< code 256) (code-char code) #\Space))))

;;Improved version of getstr with support for keymaps.
;;keymap is a hash table which maps key code to the function

(defun get-string (window keymap)
  (loop with buffer
        for c = (wgetch window)
        for kc = (gethash c keymap)
        when (= c 13) do 
           (scroll window)
           (wmove window (first (wgetyx window)) 0)
           (return (coerce (nreverse buffer) 'string))
        if kc do (setf buffer (funcall kc window buffer))
        else do (let ((cc (curses-code-char c)))
                  (case c
                    (8 (c-waddch window 8)
                       (waddch window #\Space)
                       (c-waddch window 8)
                       (pop buffer))
                    (t (waddch window cc) (push cc buffer))))))


;;REPL Console

(defclass curses-out (fundamental-character-output-stream 
                      trivial-gray-stream-mixin)
  ((window :initarg :window :accessor window)
   (height :initarg :height :accessor height)))

(defclass curses-in (fundamental-character-input-stream
                     trivial-gray-stream-mixin)
  ((window :initarg :window :accessor window)
   (height :initarg :height :accessor height)
   (keymap :initarg :keymap :accessor keymap)
   (buffer :initform nil :accessor buffer)))

(defmethod stream-write-char ((s curses-out) c)
  (if (or (char-equal c #\Newline) (char-equal c #\Return))
      (progn (scroll (window s)) (wmove (window s) (1- (height s)) 0))
      (waddch (window s) c)))

(defmethod stream-line-column ((s curses-out))
  (second (wgetyx (window s))))

(defmethod stream-line-column ((s curses-in))
  (second (wgetyx (window s))))

(define-condition out-of-repl (error) ())

(defcfun "wechochar" :int (window :pointer) (char :int))

(defun refill-buffer (s string)
  (loop with b = (list #\Return)
        for i from (1- (length string)) downto 0
        do (push (char string i) b)
        finally (setf (buffer s) b))
  (pop (buffer s)))

(defmethod stream-read-char ((s curses-in))
  (if (buffer s)
      (pop (buffer s))
      (refill-buffer s (get-string (window s) (keymap s)))))

;;  (let* ((c (wgetch (window s)))
;;         (cc (curses-code-char c)))
;;    (cond ((= c 27) (error 'out-of-repl))
;;          ((= c 13) 
;;           (scroll (window s)) (wmove (window s) (1- (height s)) 0) cc)
;;          (t (c-waddch (window s) c) cc))))


#-unix(defcfun ("PDC_ungetch" ungetch) :int (char :int))

(defmethod stream-unread-char ((s curses-in) c)
  ;;(ungetch (char-code c)) 
  (push c (buffer s))
  (c-waddch (window s) 8))

(defvar *repl-history* nil)
  
(defun repl-console (height &aux (repl-keymap (make-hash-table)))
  (attrset :cwhite)
  (loop with str1 = (format nil "~80<~>")
        and str2 = (format nil "`~79,1,0,'=<[Cur/Con]~>")
        for y from 1 to height
        do (mvaddstr (1- y) 0 str1)
           (mvaddstr y 0 str2)
           (refresh))
  (let* ((wnd (derwin *stdscr* height 0 0 0))
         (*standard-input* (make-instance 'curses-in 
                                          :window wnd 
                                          :height height
                                          :keymap repl-keymap))
         (*standard-output* (make-instance 'curses-out
                                           :window wnd :height height))
         (*print-escape* nil))
    (flet ((prompt ()
             (wattrset wnd :cgray)
             (mvwaddstr wnd (1- height) 0 (package-name *package*)) 
             (waddch wnd #\>) (waddch wnd #\Space))
           (pr (x) (print x) (scroll wnd))
           (rd () (let ((r (read))) (push r *repl-history*) r)))
      ;;repl-keymap
      (setf (gethash 27 repl-keymap) ;;ESC exits 
            (lambda (window buffer)
              (declare (ignore window buffer))
              (error 'out-of-repl)))
      (setf (gethash 259 repl-keymap) ;;UP arrow for previous command
            (lambda (window buffer)
              (if *repl-history*
                  (let ((pop (format nil "~s" (pop *repl-history*))))
                    (scroll window)
                    (funcall #'prompt)
                    (waddstr window pop)
                    (nreverse (coerce pop 'list)))
                  buffer)))
      (scrollok wnd t)
      (loop (prompt)
         (handler-case  (pr (eval (rd)))
           (out-of-repl () (delwin wnd) (return-from repl-console))
           #-(or)(error (c) (format t "~a~%" c) (scroll wnd)))))))
  