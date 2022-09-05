(defun notify (title &key message)
  (run-program "/usr/bin/notify-send" (list title message)))
