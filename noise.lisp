(load "/home/pixel/prog/lispUtilities/utilities.lisp")
(load "/home/pixel/prog/lispUtilities/math/vector.lisp")
(load "/home/pixel/prog/lispUtilities/random.lisp")

(defun pseudo-random-float (x y &optional (seed 0))
  "Returns a value between -1.0 and 1.0, based on the value of x, y and the seed"
  (sin (squirrel3-2d x y seed)))

(defun pseudo-random-angle (x y &optional (seed 0))
  "Returns a value between 0 and 2*pi, based on the values of x, y and the seed"
  (change-range (pseudo-random-float x y seed) -1 1 0 (* 2 pi)))

(defun angle-to-vector (theta)
  (list (sin theta) (cos theta)))

(defun get-corners (x y)
  "Given the coordinates x,y, returns the corners of the square containing x,y"
  (list
   (list (floor x)      (floor y))
   (list (floor (1+ x)) (floor y))
   (list (floor x)      (floor (1+ y)))
   (list (floor (1+ x)) (floor (1+ y)))))

(defun get-corner-gradients (x y &optional (seed 0))
  "Returns the pseudo-random values at the corners of the square containing x,y. The values change depending on the seed"
  (mapcar #'angle-to-vector
   (mapcar
    (lambda (i)
      (pseudo-random-angle (elt i 0) (elt i 1) seed))
    (get-corners x y))))

(defun get-offset-vectors (x y)
  "Returns the displacement vectors from x,y to the corners of the square containing the coordinate"
  (let ((corners (get-corners x y)))
    (mapcar (lambda (corner) (vector-sub (list x y) corner)) corners)))

(defun get-dot-products (x y &optional (seed 0))
  "Returns the dot-product between the pseudo-random gradiants and the displacement vector at the corners of the square containing x,y. The values change depending on the seed"
  (let ((offsets (get-offset-vectors x y))
	(gradiants (get-corner-gradients x y seed)))
    (map 'list (lambda (offset gradiant) (vector-dot offset gradiant)) offsets gradiants)))

(defun easing (x)
  "Easing function which has zero first and second derivatives at both x=0 and x=1"
  (+ (* 6 x x x x x) (* -15 x x x x) (* 10 x x x)))

(defun perlin (x y &optional (seed 0))
  (let ((dots (get-dot-products x y seed))
	(u (- x (floor x)));Can't bother with multiple returns, so this will have to do
	(v (- y (floor y))))
    (lerp
     (lerp (elt dots 0) (elt dots 1) (easing u))
     (lerp (elt dots 2) (elt dots 3) (easing u))
     (easing v))))

(defun to-file (seed)
  (dotimes (i 512)
    (dotimes (j 512)
      (dotimes (z 3)
	(append-to-file (floor (change-range (perlin (+ 0.03 (* 0.007 i)) (+ 0.03 (* 0.007 j)) seed) -1 1 0 255)) "perlin.ppm")))))
