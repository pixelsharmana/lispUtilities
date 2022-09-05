(defun radian-to-degree (radians)
  (* radians (/ 180 pi)))



(defun point-inside-rectangle-p (point-x point-y rectangle-x rectangle-y rectangle-width rectangle-height)
  (when (and
	 (>= point-x rectangle-x)
	 (>= point-y rectangle-y)
	 (<= point-x (+ rectangle-x rectangle-width))
	 (<= point-y (+ rectangle-y rectangle-height)))
      t))

(defun square (x)
  "Returns x squared"
  (* x x))

;Only handles 2D for now
(defun distance-pp (point1 point2)
  "Returns the euclidean distance between point1 and point2"
  (sqrt
   (+ (square (- (elt point1 0) (elt point2 0)))
      (square (- (elt point1 1) (elt point2 1))))))

(defun points-to-line (point1 point2)
  "Returns a list of the slope 'm' and the y intercept 'b' in the line equation y=mx+b"
  (cond ((eq (elt point1 1) (elt point2 1));If y are the same
	 (list 0 (elt point1 1)));Then slope is 0
	((eq (elt point1 0) (elt point2 0));If x are the same
	 'infinity);Then slope is infinite
	(t
	 (let ((slope (/ (- (elt point1 1) (elt point2 1))
			 (- (elt point1 0) (elt point2 0)))))
	   (list
	    slope
	    (if (zerop slope)
		(elt point1 1)
		(- (elt point1 1) (* slope (elt point1 0)))))))))

(defun line-line-intersection (line1 line2)
  (let ((x 
	  (*
	   (/ (- (elt line1 0) (elt line2 0)))
	   (- (elt line2 1) (elt line1 1)))))
    (list x (+ (* (elt line1 0) x) (elt line1 1)))))

(defun line-line-angle (line1 line2)
  (/ (- (elt line1 0) (elt line2 0))
     (1+ (* (elt line1 0) (elt line2 0)))))
