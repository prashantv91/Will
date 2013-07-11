; Braainnss..

(in-package :will.brains)

(defclass-2 basic-brain ()
            ; Base class for brains.
            ())

(defgeneric update (brain info)
  ; Function that the simulator calls to send updates to.
  )

(defgeneric get-move (brain)
  ; Function that the simulator calls to request a move.
  )


(defclass-2 client-handler (basic-brain)
            ; Interfaces with the client.
            ((socket nil)))

(defmethod update ((brain client-handler) info)
  ; Currently sends everything apdiye to the client.
  (defun encode-update (info_)
    ; Encodes the update into something that can be sent to the client.
    info_)

  (let ((msg (encode-update info))
        (msg-type 'update))
    (send-msg (socket brain) msg-type msg)))

(defmethod get-move ((brain client-handler))
  ; Requests the client for a move, receives and returns it.
  (defun decode-move (move)
    ; Decodes the string received from the client into a move and returns it.
    move)

  (progn
    (send-msg (socket brain) 'get-move "")
    (decode-move (receive-msg (socket brain)))))
        


