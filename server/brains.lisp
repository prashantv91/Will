; Braainnss..

(in-package :will.brains)

(defclass-2 basic-brain ()
            ; Base class for brains.
            ())

(defgeneric update (brain tim info)
  ; Function that the simulator calls to send updates to.
  ; This notifies @brain of update @info at time @tim
  )

(defgeneric get-move (brain tim)
  ; Function that the simulator calls to request a move.
  ; This requests @brain for a move at time @tim.
  ; Supposed to return a move.
  )


(defclass-2 client-handler (basic-brain)
            ; Interfaces with the client.
            ((socket nil)))

(defmethod update ((brain client-handler) tim info)
  ; Currently sends everything apdiye to the client.
  (defun encode-update (tim info_)
    ; Encodes the update into something that can be sent to the client.
    tim
    info_)

  (let ((msg (encode-update tim info))
        (msg-type 'update))
    (send-msg (socket brain) msg-type msg)))

(defmethod get-move ((brain client-handler) tim)
  ; Requests the client for a move, receives and returns it.
  (defun decode-move (move)
    ; Decodes the string received from the client into a move and returns it.
    move)

  (progn
    (send-msg (socket brain) 'get-move "")
    (decode-move (receive-msg (socket brain)))))
        


