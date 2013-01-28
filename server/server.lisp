; Contains functions that handle networking for the server.

(use-package :sb-bsd-sockets)

(defstruct packet 
  ; Structure to store network packets.
  type
  length
  data)

(defun receive-full (socket len)
  ; To ensure everything is received.
  (let ((msg (socket-receive socket nil len)))
    (if (= (length msg) 0)
      ""
      (if (< (length msg) len)
        (concatenate 'string msg (receive-full socket (- len (length msg))))
        msg))))

(defun send-full (socket msg)
  ; To ensure everything is sent.
  (let* ((len (length msg))
         (len-sent (socket-send socket msg len)))
    (if (= len-sent 0)
      nil
      (if (< len-sent len)
        (send-full socket (subseq msg len-sent))))))

(defun start-server (server-port &optional (backlog 5))
  ; Create a socket listening on @port with max queue length @backlog and return it.
  (let ((server-socket (make-instance 'inet-socket :type :stream :protocol :tcp)))
    (socket-bind server-socket
                 (car (host-ent-addresses (get-host-by-name (machine-instance))))
                 server-port)
    (socket-listen server-socket backlog)
    server-socket))

(defun stop-server (server-socket)
  ; For completeness.
  (socket-close server-socket))

(defun get-client (server-socket)
  ; Waits for and accepts an incoming client connection, returns the new socket.
  (socket-accept server-socket))

(defun receive (socket)
  ; Receives and returns a packet object.
  (let* ((msg-type (str-to-int (socket-receive socket nil 1)))
         (msg-len  (str-to-int (socket-receive socket nil 4)))
         (msg      (receive-full socket msg-len)))
    (make-packet :type msg-type :length msg-len :data msg)))

(defun send (socket pkt)
  ; Sends a packet as specified by the packet object @pkt.
  (unless (not (packet-p pkt))
    (socket-send socket (int-to-str (packet-type pkt) 1) 1)
    (socket-send socket (int-to-str (packet-length pkt) 4) 4)
    (send-full socket (packet-data pkt))))

(let ((server-socket nil) (server-running? nil) (client-socket nil))        ; Clients management soon to come.

  )


;(setf sock (start-server 2218))
;
;(setf sock2 (get-client sock))
;
;(send-full sock2 "Hello")
;
;(receive-full sock2 10)
;
;(setf pk (receive sock2))
;
;(send sock2 pk)
;
;(* pk 2)
;
;(socket-close sock2)
