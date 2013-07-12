; Functions for animate objects. 

(in-package :will.animate)

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

