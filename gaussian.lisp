(defconstant e 2.71828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746)

(defun gaussian-Distribution (mean standard-deviation)
  (lambda (x)
    (*
     (/ (* standard-deviation (sqrt (* 2 pi))))
     (expt e 
	   (* -1/2 (expt (/ (- x mean) standard-deviation) 2))))))

(defun rejection-sample (distribution deviation &optional (mean 0))
    (let* ((max-y (funcall distribution mean))
	   (trial-point (list (- deviation (random (* 2.0d0 deviation))) (random max-y))))
      (if (> (elt trial-point 1) (funcall distribution (elt trial-point 0)))
	  (elt trial-point 0)
	  (rejection-sample distribution deviation mean))))
