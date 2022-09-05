(load "/home/pixel/prog/lispUtilities/utilities.lisp")

(defun smaller-than-index (sequence value &optional (comparaison-function #'<))
  "Finds the index of the first element for which value is smaller than it"
  (let ((length (length sequence)))
    (labels ((smaller-helper (sequence value index)
	       (if (null sequence)
		   length
		   (if (funcall comparaison-function (car sequence) value)
		       (smaller-helper (cdr sequence) value (1+ index))
		       index))))
      (smaller-helper sequence value 0))))

(defun insertion-sort (sequence value &optional (comparaison-function #'<))
  "Inserts an element in a list according to the comparaison-function (Defaults to <)"
  (elt-insert sequence (smaller-than-index sequence value comparaison-function) value))
