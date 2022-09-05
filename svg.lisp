(load "/home/pixel/prog/lispUtilities/utilities.lisp")

(defun colour-to-svg (colour)
  (format nil "fill:rgb(~a,~a,~a);" (elt colour 0) (elt colour 1) (elt colour 2)))

(defun half (x)
  (/ x 2.0))

(defun circle (x y radius &optional (colour (list 255 0 0)))
  (list :type 'circle :x (+ x (half radius)) :y (+ y (half radius)) :radius radius :colour colour))

(defun rect (x y width height &optional (colour (list 255 0 0)))
  (list :type 'rectangle :x x :y y :width width :height height :colour colour))

(defun svg-print-circle (circle)
  (format nil "<circle cx=\"~a\" cy=\"~a\" r=\"~a\" style=\"~a\"/>"
	  (getf circle :x)
	  (getf circle :y)
	  (getf circle :radius)
	  (colour-to-svg (getf circle :colour))))

(defun svg-print-rect (pill)
  (format nil "<rect x=\"~a\" y=\"~a\" width=\"~a\" height=\"~a\" style=\"~a\"/>"
	  (getf pill :x)
	  (getf pill :y)
	  (getf pill :width)
	  (getf pill :height)
	  (colour-to-svg (getf pill :colour))))

(defun svg-print (object)
  (cond ((eq (getf object :type) 'circle) (svg-print-circle object))
	((eq (getf object :type) 'rectangle) (svg-print-rect object))
	(t (error "Unknown object type"))))

(defun pill (x y width height &optional (colour (list 255 0 0)))
  (list
   (circle (+ x (/ height 4.0)) (+ y (/ height 4.0)) (half height) colour)
   (circle (+ x (/ height 4.0) (- width height)) (+ y (/ height 4.0)) (half height) colour)
   (rect (+ x (/ height 2.0)) y (- width height) height colour)
   ))

(defun svg-wrap (width height string)
  (format nil "<?xml version=\"1.0\" encoding=\"UTF-8\"?>~%<svg version=\"1.1\" viewBox=\"0 0 ~a ~a\" xmlns=\"http://www.w3.org/2000/svg\">~%~a~%</svg>" width height string))

(defun pill-quick-render (x y width height &optional (colour (list 255 0 0)))
  (format nil "~a" (svg-wrap 1000 1000 (reduce (lambda (a b) (concatenate 'string a b)) (mapcar #'svg-print (pill x y width height colour))))))

(defun convert ()
  (run-program "/usr/bin/inkscape" (list "-w" "1000" "-h" "1000" "test2.svg" "-o" "test.png")))

(defun do-all (x y width height &optional (colour (list 255 0 0)))
  (progn
    (save-file (pill-quick-render x y width height colour) "test2.svg")
    (convert)))
