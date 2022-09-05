(load "/home/pixel/prog/lispUtilities/utilities.lisp")

;symetricp

(defun coords-2d-to-1d (x y cols)
  "Transforms a 2d coordinate into a 1d one"
  (+ x (* y cols)))

(defun coords-1d-to-2d (n cols)
  "Transforms a 1d coordinate into a 2d one"
  (list (mod n cols) (truncate (/ n cols))))

(defun matrix-size (matrix)
  "Returns the number of elements in matrix"
  (* (getf matrix :cols) (getf matrix :rows)))

(defun matrix-from-list (rows cols list)
  "Creates a matrix from a list and dimensions. The cols times rows needs to be equal to the length of the list"
  (if (not (eq (* cols rows) (length list)))
      (error "The dimensions of the matrix needs to be equal to the supplied list")
      (list :rows rows
	    :cols cols
	    :matrix list)))

(defun matrix-from-matrix (matrix list)
  "Create a new matrix from a list, using the dimensions of the supplied matrix"
  (matrix-from-list (getf matrix :cols) (getf matrix :rows) list))

(defun matrix-create (rows cols &optional (initial-element 0))
  "Create a matrix of the given dimensions"
  (matrix-from-list cols rows (make-list (* cols rows) :initial-element initial-element)))

(defun matrix-set (matrix m n value)
  "Returns a new matrix with the element at m,n changed to value"
  (matrix-from-list
   (getf matrix :cols)
   (getf matrix :rows)
   (elt-change (getf matrix :matrix) (coords-2d-to-1d m n (getf matrix :cols)) value)))

(defun matrix-get (matrix m n)
  "Returns the element at m,n"
  (elt-bound (getf matrix :matrix) (coords-2d-to-1d m n (getf matrix :cols))))

(defun matrix-get-row (matrix n)
  "Returns the nth row"
  (let ((cols (getf matrix :cols)))
    (subseq (getf matrix :matrix) (* n cols) (+ (* n cols) cols))))

(defun matrix-get-column (matrix m)
  "Returns the mth column"
  (let ((result nil))
    (dotimes (i (getf matrix :rows))
      (ntack result (matrix-get matrix m i)))
    result))

(defun matrix-get-rows (matrix)
  "Returns the rows of a matrix in a list in order"
  (let ((result nil))
    (dotimes (i (getf matrix :rows))
      (ntack result (matrix-get-row matrix i)))
    result))

(defun matrix-get-columns (matrix)
  "Returns the columns of a matrix in a list in order"
  (let ((result nil))
    (dotimes (i (getf matrix :cols))
      (ntack result (matrix-get-column matrix i)))
    result))

(defun matrix-add (a b)
  "Returns the sum of matrices a and b. The dimensions of the matrices need to be equal"
  (if (not (eq (matrix-size a) (matrix-size b)))
      (error "The dimensions of the matrices to be added must be equal to one another")
      (matrix-from-matrix a (map 'list #'+ (getf a :matrix) (getf b :matrix)))))

(defun matrix-scalar-multiplication (matrix scalar)
  "Returns a new matrix multiplied by the given scalar"
  (matrix-from-matrix matrix (mapcar (lambda (i) (* i scalar)) (getf matrix :matrix))))

(defun matrix-transpose (matrix)
  "Returns the transpose of the supplied matrix"
  (matrix-from-list (getf matrix :rows) (getf matrix :cols) (flatten (matrix-get-columns matrix))))

(defun matrix-multiply (a b)
  "Return the product of matrices a and b. The number of columns of matrix a must be the equal to the number of rows of matrix b"
  (unless (eq (getf a :cols) (getf b :rows))
    (error "The number of columns of matrix a is not equal to the number of rows of matrix b"))
  (defun mul-row-col (m n)
    (reduce #'+ (map 'list #'* (matrix-get-row a m) (matrix-get-column b n))))
  (let ((result nil))
    (dotimes (x (getf a :rows))
      (dotimes (y (getf b :cols))
	(ntack result (mul-row-col x y))))
    (matrix-from-list (getf b :cols) (getf a :rows) result)))

(defun matrix-print (matrix)
  (dotimes (i (getf matrix :rows))
    (format t "~a~%" (matrix-get-row matrix i))))
