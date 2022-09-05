(defun sample ()
  "Outputs a number between -1 and 1"
  (1- (random 2.0)))

(defun square (x)
  (* x x))

(defun trial ()
  "Is the point in the unit circle?"
  (<= (+ (square (sample)) (square (sample))) 1))

(defparameter inside 0)
(defparameter total 0)

(defun thing (x)
  (dotimes (i x)
    (when (trial)
      (incf inside))
    (incf total)))
