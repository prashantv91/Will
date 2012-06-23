(asdf:oos 'asdf:load-op :gambol)
(use-package :gambol)

(*- (a 1 2))
(*- (a 3 2))

(*- (a 2 1))

(pl-retract '((a 2 ??)))

(??- (a ?x ??) (a ?? ?x) )

(*- (fact 0 1))

(*- (fact ?x ?y) 
    (is ?x2 (lop (- ?x 1)))
    (fact ?x2 ?y2)
    (is ?y (lop (* ?y2 ?x))))

(?- (fact 1 ?x))

(clear-rules)

(pl-solve-all '((a ?x ?y)))

(*- (a (1 2) 1))

(defun echo (x) x)

(*- (not ?p) ?p (cut) (fail))
(*- (not ?p))

(*- (alive ?x) (not (kill ?y ?x)))

(*- (action ?x) (lisp (format t "~A" ?x)))

(*- (kill me ??) (action (hire me)))

(*- (kill dragon sheep) (alive dragon))

(*- (save ?x) (kill ?y ?x) (kill ?z ?y))

(pl-solve-all '((kill dragon sheep)))

(pl-solve-all '((save sheep)))

(pl-solve-all '((alive sheep)))

(?- (action (hire me)))

(let ((default-rulebase (current-rulebase)))
  (with-rulebase (make-rulebase)
    (*- (fibonacci 0 1))
    (*- (fibonacci 1 1))
    (with-rulebase default-rulebase
      (pl-solve-one '((fibonacci 0 1))))))


(*- (app (?h1 . ?t1) ?l2 (?h1 . ?t3))
    (app ?t1 ?l2 ?t3))
(*- (app nil ?l2 ?l2))

(*- (pr (?a . ?b))
    (lisp (format t "~A ~A" ?a ?b)))


