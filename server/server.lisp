; Contains functions that handle networking for the server.

(in-package :will.server)

(defparameter protocol-map nil)     ; Mapping from message types to integers that are sent over network.

(defstruct packet 
  ; Structure to store network packets.
  type_
  length_
  data)

(defun pr-to-int (msg-type)
  (cdr-assoc msg-type protocol-map))

(defun int-to-pr (msg-type-num)
  (cdr-rassoc msg-type-num protocol-map))

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
  (let* ((msg-type (int-to-pr (str-to-int (socket-receive socket nil 1))))
         (msg-len  (str-to-int (socket-receive socket nil 4)))
         (msg      (receive-full socket msg-len)))
    (make-packet :type_ msg-type :length_ msg-len :data msg)))

(defun send (socket pkt)
  ; Sends a packet as specified by the packet object @pkt.
  (unless (not (packet-p pkt))
    (socket-send socket (int-to-str (pr-to-int (packet-type_ pkt)) 1) 1)
    (socket-send socket (int-to-str (packet-length_ pkt) 4) 4)
    (send-full socket (packet-data pkt))))

(defun receive-cond (socket msg-type)
  ; Eats packets until one of type @msg-type is found and returns its msg.
  (let ((pkt (receive socket)))
    (if (eql (packet-type_ pkt) msg-type)
      (packet-data pkt)
      (receive-cond socket msg-type))))

(defun receive-msg (socket)
  ; Receives a packet and returns only the msg.
  (socket-receive socket nil 1)
  (let* ((msg-len  (str-to-int (socket-receive socket nil 4)))
         (msg      (receive-full socket msg-len)))
    msg))   

(defun send-msg (socket msg-type msg)
  ; Computes message length and sends, without need for packet object.
  (socket-send socket (int-to-str (pr-to-int msg-type) 1) 1)
  (socket-send socket (int-to-str (length msg) 4) 4)
  (send-full socket msg))


(defun load-protocol-file (file-name)
  ; Fills the table protocol-map, which is a mapping from message types to integers.
  (with-open-file (file 
                    (make-pathname :name file-name :directory '(:relative "."))
                    :direction :input)
    (do ((pair (read file nil 'EOF)
               (read file nil 'EOF)))
      ((eql pair 'EOF))
      (progn
        (setf protocol-map (acons (car pair) (cadr pair) protocol-map))
        (debug-print pair)
        (debug-print protocol-map))))) 



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
