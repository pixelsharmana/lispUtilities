(load "/home/pixel/prog/lispUtilities/utilities.lisp")

(defun get-memory ()
  (let ((proc
	  (run-program "/home/pixel/prog/lispUtilities/getMem.sh" nil :output :stream :wait nil)))
    (read (process-output proc))))

(defun get-memory-human-readable ()
  (let ((temp (mapcar (lambda (i) (/ (/ i 1024.0) 1024.0)) (get-memory))))
    (format t "~a Mb used memory / ~a Mb total memory" (- (elt temp 0) (elt temp 1)) (elt temp 0))))
