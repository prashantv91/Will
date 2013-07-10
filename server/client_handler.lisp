; Class that interacts with the client. Decides what gets sent to it.
; Separated from server code because the caching may get complicated later.

(in-package :will.server)

(defclass-2 client-handler ()
  ((socket)
   (monster)))



