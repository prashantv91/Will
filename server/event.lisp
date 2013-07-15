; Classes for events, and functions that make them happen.

(in-package :will.simulator)

(defclass-2 event ()
            )

(defgeneric happen (tim eve)
  ; Makes event @eve happen at time @tim.
  ; Returns a list of (time . event) conses that it wishes to have inserted into the event queue.
  )


