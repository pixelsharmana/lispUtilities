(defun coords-2d-to-1d (x y cols)
  "Transforms a 2d coordinate into a 1d one"
  (+ x (* y cols)))

(defun coords-1d-to-2d (n cols)
  "Transforms a 1d coordinate into a 2d one"
  (list (mod n cols) (truncate (/ n cols))))

(defun ppm-image (width height)
  (list ':width width ':height height))
