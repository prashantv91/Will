; Starts everything up.

(in-package :will.main)

(defparameter *server-port* nil)    ; Server port.
(defparameter *max-clients* nil)    ; Max. number of clients.

(load-config-file "config")     ; Hard-coded.
(load-protocol-file "protocol")

(let ((client-sockets nil))

  (defun clear-clients ()
    ; Temporary. Clears @client-sockets.
    (setf client-sockets nil))

  (defun server-loop (server-socket max-clients)
    ; Tail-recursive procedure that listens to clients and lets them possess monsters.
    (let ((client-sock (get-client server-socket)))
      (debug-print "Got client.")
      (if (< (length client-sockets) max-clients)
        (progn
          (debug-print "Adding client.")
          (setf client-sockets (cons client-sock client-sockets))
          (possess (receive-cond client-sock 'initialise) client-sock))))
    (server-loop server-socket max-clients)
    )

  (defun run-server (server-port max-clients)
    ; Starts server and client management.
    (let ((server-socket (start-server server-port)))
      (debug-print "Running server.")
      (server-loop server-socket max-clients)
      (stop-server server-socket)))             ; This thing still seems to be leaving the port open. Check that out.
  )

(let ((server-thread nil))
  (defun start-off ()
    ; The main function.

    (setf server-thread (make-thread #'run-server 
                                     :name "server-thread" 
                                     :arguments (list *server-port* *max-clients*)))
    ;(run-server *server-port* *max-clients*)
    )
  
  (defun kill-server ()
    ; Terminates the server thread. Not that good a thing to do.
    (terminate-thread server-thread))

  )




