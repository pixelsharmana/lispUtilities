(load "/home/pixel/prog/lispUtilities/utilities.lisp")

(defun ramp-segmentation (sequence)
  "Segments a sequence into equally spaced terms"
  (let ((length (length sequence)))
    (mapcar (lambda (i) (list (+ (/ length) (/ i length)) (elt sequence i))) (range length))))

(defun ramp (sequence value)
  "Associate a value between 0.0 and 1.0 with an element in the sequence based on their order"
  (cond ((< value 0) (car sequence))
	 ((>= value 1) (last1 sequence))
	 (t (cadr (assoc value (ramp-segmentation sequence) :test #'<)))))
