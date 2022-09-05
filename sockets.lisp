(defun create-socket (ip port &optional (type :client))
  "Prepares a TCP socket ready for talking, type accepts 'client or 'server, ip is a list of 4 values (for IPv4)"
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (if (eq type :client)
	(sb-bsd-sockets:socket-connect socket ip port)
	'whoopsie)))

(defparameter *my-socket* nil)
(defparameter *my-client-stream* nil)
(defparameter *my-server-stream* nil)
