(load "/home/pixel/prog/lispUtilities/utilities.lisp")

(defun send-message (message ip &optional (recipient "pixel"))
  (let ((message-id (create-uuid)))
    (save-file message message-id)
    (run-program "/usr/bin/rsync" (list "-cr" message-id "/home/pixel/inbox/.") :output *standard-output*)))
