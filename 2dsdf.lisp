;2d signed distance functions of many primitives
;The functions return the distance from a point to the shape described

;All primitives are centered at the origin
;You will have to transform the point to get arbitrarily rotated, translated and scaled objects

(load "vector.lisp")

(defmacro deco (point)
  "Decompose a 2d point into calls to its elements"
  `(list (elt ,point 0) (elt ,point 1)))

(defun sd-circle (point radius)
  "Returns the exact distance from a circle of radius r. A negative value means the point (x y) is inside the circle"
  (- (vector-norm (deco point)) radius))

(defun sd-box (point box)
  "Returns the exact distance from a box. A negative value means the point (x y) is inside the box."
  (let (distance (vector-norm (vector-sub point box)))
    
