; Starts everything up.

(in-package :will.main)

(defparameter *server-port* nil)
(defparameter *max-clients* nil)

(load-config-file "config")     ; Hard-coded.
(load-protocol-file "protocol")

(let ((client-sockets nil))

  (defun run-server (server-socket)
    ; Tail-recursive procedure that listens to clients and lets them possess monsters.
    (let ((client-sock (get-client server-socket)))
      (if (< (length client-sockets) *max-clients*)
        (progn
          (setf client-sockets (cons client-sock client-sockets))
          (possess (receive-cond client-sock 'initialise) client-sock))))
    (run-server server-socket))

  (defun server-work ()
    ; Starts server and client management.
    (let ((server-socket (start-server *server-port*)))
      (run-server server-socket)
      (stop-server server-socket)))
  )

(defun start-off ()
  (make-thread #'server-work :name 'server-thread))
  



