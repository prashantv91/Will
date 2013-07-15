; The simulator. 

(in-package :will.simulator)

(defparameter *monsters* nil)
(defparameter *items* nil)

(let ((event-queue (make-heap)) (global-time 0))      ; The event queue - a heap.

  (defun reset ()
    ; Resets the queue and time.
    (setf event-queue nil)
    (setf global-time 0))

  (defun set-time (tim)
    ; Sets time to @tim.
    (setf global-time tim))

  (defun schedule-event (tim eve)
    ; Inserts event @eve, which shall happen at time @tim.
    (heap-push event-queue tim eve))

  (defun do-next-event ()
    ; Makes the next event happen. Currently not used.
    (let* ((next-cons (heap-pop event-queue))
           (tim (car next-cons))
           (eve (cdr next-cons)))
      (happen tim eve)))

  (defun sim-loop ()
    ; The main simulator loop.
    (do ((next-event (heap-pop event-queue)
                     (heap-pop event-queue)))
      ((heap-empty event-queue))

      (let ((tim (car next-event))
            (eve (cdr next-event)))
        (set-time tim)

        (let ((new-events (happen tim eve)))
          (loop 
            for new-event in new-events
            do 
            (schedule-event (car new-event) 
                            (cdr new-event)))))))

  )

(defgeneric possess (monster-info client-sock)
  ; Do this after some more of the server is done - retrieve the correct
  ; monster and create a client-handler for it and perform brain transplant
  ; under mutex.
  )

(defmethod possess (monster-info client-sock)
  ; The most generic possession. Write a :before method.
  (debug-print "Possessing.")
  (debug-print monster-info)
  ; (debug-print client-sock)

  )


