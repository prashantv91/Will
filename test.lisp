(asdf:operate 'asdf:load-op 'cffi)

(asdf:operate 'asdf:load-op 'trivial-gray-streams)

(load "curses.lisp")

(in-package :curses)

(connect-console)

(erase)

(attrset :cpurple)

(mvaddstr 2 2 "Hello World!")

(refresh)

(+ 1 2)
