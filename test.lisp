(in-package sb-bsd-sockets)

(setf server (make-instance 'inet-socket :type :stream :protocol :tcp))

(socket-bind server
             (car (host-ent-addresses (get-host-by-name (machine-instance))))
               2218)

(socket-listen server 5)

(setf newsock (socket-accept server))

(socket-send newsock "Hello World." 13)

(socket-receive newsock nil 20)

(socket-peername newsock)



