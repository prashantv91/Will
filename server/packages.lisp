; Package definitions.

(defpackage :will.base
  (:use :common-lisp)
  (:export :int-to-str      :str-to-int     :defclass-2
           :get-last        :cdr-assoc      :load-config-file   :*DEBUG*))


(defpackage :will.server
  (:use :sb-bsd-sockets
        :will.base)
  (:export  :start-server   :stop-server    :get-client
            :receive        :send))

(defpackage :will.animate
  (:use :will.base))
  
(defpackage :will.main
  (:use :will.server
        :will.animate))

