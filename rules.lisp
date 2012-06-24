; Functions for processing rules.
(load "base.lisp")

(asdf:oos 'asdf:load-op :gambol)
(use-package :gambol)

(defparameter *knowledge-dir* "knowledge" "Relative path to directory with rule files.")
(defparameter *basic-rules-file* "basic" "Do not pre-process rules from this file.")
(defparameter *rules-files* '("basic" "test") "Rules files.")

(defun gen-logic-var ()
  ; Generate a symbol to be used as logic variable.
  (gentemp "?G"))

(defun add-rule (rule)
  ; Add @rule to rulebase.
  (pl-assert rule))

(defun extract-action-var (lst)
  ; Returns symbol of logic variable for action list in given rule or goals (@lst).
  (get-last lst))

(defun get-rule-body (lst)
  (if (atom (car lst))
    nil
    (cons (car lst) (get-rule-body (cdr lst)))))

(defun process-rule (rule)
  ; Processes @rule before it is added to rulebase, adding action list.
  ; ((a 1) (b 2) ... (c 3)) -> ((a 1 #1 #2) (b 2 (#1 #3)) ... (c 3 #n #2))
  (let* ((firstarg (gen-logic-var)) 
         (inter-body (process-rule-body (cdr rule) firstarg))
         (lastarg (extract-action-var inter-body))
         (body (get-rule-body inter-body)))          ; Find better way to do this.
    (dbgprint "Processing: ~A~%~%" rule)
    (cons (append (car rule) (list firstarg lastarg)) body)))

(defun process-rule-body (rule arg)
  ; Processes body of rule, adding action list.
  ; ((a 1) (b 2)) -> ((a 1 #1 #2) (b 2 #2 #3) #3)
    ; (cons `(lisp (dbgprint "~A~%" ,arg))
  (if (null rule)
    (list arg)
    (let* ((rule-part (car rule)) 
           (action-part (cadr rule-part)) 
           (nextarg (gen-logic-var)))
      (case (car rule-part) 
        ('lisp
         (cons rule-part
               (process-rule-body (cdr rule) arg)))
        ('lop
         (cons rule-part
               (process-rule-body (cdr rule) arg)))
        ('cut
         (cons rule-part
               (process-rule-body (cdr rule) arg)))
        ('action
         (cons (list 'rev-is `(list ,action-part ,@arg) nextarg)
               (process-rule-body (cdr rule) nextarg)))
        ('if
         (cons (append (cadr rule-part) (list nil '??))
               (process-rule-body (cdr rule) arg)))
        ('not  
         (case (car (cadr rule-part))
           ('lop
            (cons (list 'not (cadr rule-part))
                  (process-rule-body (cdr rule) arg)))
           (otherwise
             (cons (list 'not (append (cadr rule-part) (list nil '??)))
                   (process-rule-body (cdr rule) arg)))))
        (otherwise
          (cons (append rule-part (list arg nextarg))
                (process-rule-body (cdr rule) nextarg)))))))

(defun read-rules (&optional (file-list *rules-files*))
  ; Reads rules from files in @file-list and add to current rulebase after processing. 
  (unless (null file-list)
    (let ((file-name (car file-list)))
      (with-open-file (file 
                        (make-pathname 
                          :directory (list :relative  *knowledge-dir*)
                          :name file-name)
                        :direction :input)
        (do ((rule (read file nil 'EOF)
                   (read file nil 'EOF)))
          ((eql rule 'EOF))
          (if (equal file-name *basic-rules-file*)    ; Do not pre-process rules from this file.
            (add-rule rule)
            (add-rule (process-rule rule))))))
    (read-rules (cdr file-list))))

(defun refresh-rules ()
  ; Refreshes from files and prints rules in rulebase.
  (progn
    (clear-rules)
    (read-rules)
    (print-rules)))

(defun process-goals (goals &optional (arg nil))
  ; Adds action variables to list of goals.
  ; Should this be relegated to process-rule-body? Probably.
  (unless (null goals)
    (let ((goal-part (car goals)) (nextarg (gen-logic-var)))
      (cons (append goal-part (list arg nextarg))
            (process-goals (cdr goals) nextarg)))))

(defun get-all-action-seqs (goals)
  ; Returns list of all possible action sequences to achieve list of goals.
  ; Note that this can't tell whether goals have already been achieved.
  (let* ((new-goals (process-goals goals)) 
         (action-var (extract-action-var new-goals)))
    (mapcar #'(lambda (lst) (cdr-assoc action-var lst))
            (pl-solve-all new-goals))))

(let ((prev-goals nil) (action-var nil))
  (defun get-an-action-seq (goals)
    ; Gets an action sequence for @goals, different from the previous one if called twice with same @goals.
    ; Stop calling when it returns NIL.
    (if (not (equal prev-goals goals))
      (let* ((new-goals (process-goals goals))) 
        (setf prev-goals goals)
        (setf action-var (extract-action-var new-goals))
        (cdr-assoc action-var (pl-solve-one new-goals)))
      (cdr-assoc action-var (pl-solve-next))))
  (defun get-next-action-seq ()
    ; Returns next action sequence for the @goals in last call to get-an-action-seq.
    (cdr-assoc action-var (pl-solve-next))))



