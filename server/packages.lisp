; Package definitions.

(require :sb-bsd-sockets)
;(require :sb-thread)

(defpackage :will.base
  (:use :common-lisp)
  (:export :int-to-str      :str-to-int     :defclass-2         :get-last
           :cdr-assoc       :cdr-rassoc     :load-config-file   :*DEBUG*
           :debug-print))

(defpackage :will.heap
  (:use :common-lisp
        :will.base)
  (:export  :make-heap      :heap-push      :heap-pop           :heap-empty     
            :heap-size))

(defpackage :will.server
  (:use :common-lisp
        :sb-bsd-sockets
        :will.base)
  (:export  :start-server   :stop-server    :get-client         :load-protocol-file
            :receive        :send           :receive-msg        :send-msg
            :receive-cond))

(defpackage :will.brains
  (:use :common-lisp
        :will.base
        :will.server)
  (:export  :basic-brain    :client-handler :update         :get-move))

(defpackage :will.animate
  (:use :common-lisp
        :sb-thread
        :will.base
        :will.server
        :will.brains)
  (:export  :*species*      :dog))
  
(defpackage :will.map
  (:use :common-lisp
        :will.base))

(defpackage :will.simulator
  (:use :common-lisp
        :sb-thread
        :will.base
        :will.heap
        :will.animate)
  (:export  :possess))

(defpackage :will.main
  (:use :common-lisp
        :sb-thread
        :will.base
        :will.server
        :will.animate
        :will.simulator)
  (:export  :start-off))

