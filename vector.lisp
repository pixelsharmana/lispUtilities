(defun vector? (vector)
  "Tests if a given form is considered a vector"
  (when (listp vector)
    (notany #'listp vector)))

(defun vector-create (dimensions)
  "Creates a vector of the given dimension"
  (make-list dimensions :initial-element 0))

(defun vector-dimension (vector)
  (length vector))

(defun vector-scalar (vector scalar)
  "Multiplies the vector by a scalar"
  (mapcar (lambda (i) (* i scalar)) vector))

(defun vector-invert (vector)
  "Reverses the direction of a vector"
  (vector-scalar vector -1))

(defmacro vector-op (op vector1 vector2)
  "Applies an operation using a pair of values from the first vector to the second vector"
  `(map 'list #',op ,vector1 ,vector2))

(defun vector-add (vector1 vector2)
  "Adds two vector together, element-wise"
  (vector-op + vector1 vector2))

(defun vector-sub (vector1 vector2)
  "Substracts vector2 from vector1"
  (vector-add vector2 (vector-invert vector1)))

(defun vector-norm (vector)
  "The length in euclidean space of the vector"
  (sqrt (reduce #'+ (mapcar (lambda (i) (* i i)) vector))))

(defun vector-normalize (vector)
  "Transform a vector into a unit vector"
  (if (zerop (vector-norm vector))
      vector
      (vector-scalar vector (/ (vector-norm vector)))))

(defun vector-dot (vector1 vector2)
  "The dot product of two vectors"
  (reduce #'+ (vector-op * vector1 vector2)))

(defun vector-cross (vector1 vector2)
  "The cross product of two vectors"
  (list (- (* (elt vector1 1) (elt vector2 2)) (* (elt vector1 2) (elt vector2 1)))
	(- (* (elt vector1 2) (elt vector2 0)) (* (elt vector1 0) (elt vector2 2)))
	(- (* (elt vector1 0) (elt vector2 1)) (* (elt vector1 1) (elt vector2 0)))))

(defun vector-angle (vector1 vector2)
  "The angle between two vectors"
  (acos (/
	 (vector-dot vector1 vector2)
	 (* (vector-norm vector1) (vector-norm vector2)))))
