(load "/home/pixel/prog/lispUtilities/utilities.lisp")

(defun rtn-no-step-left (input)
  (every #'stringp input))

(defun rtn-substitution (symbol grammar)
  (if (stringp symbol)
      symbol
      (elt-random (cdr (assoc symbol grammar)))))

(defun rtn-pass (starting-symbols grammar)
  (flatten (mapcar (lambda (i) (rtn-substitution i grammar)) starting-symbols)))

(defparameter *grammar*
  '((sentence (article adjective noun))
    (article article2)
    (article2 "the")
    (noun "cat" "dog" "bird")
    (adjective "big" "small" "brown")))
