;Rational numbers

(defun rational-create (numerator denominator)
  (list numerator denominator))

(defun rational-numerator (rational)
  (elt rational 0))

(defun rational-denominator (rational)
  (elt rational 1))

(defun rational-add (x y)
  (rational-create (+ (* (rational-numerator x) (rational-denominator y))
		      (* (rational-numerator y) (rational-denominator x)))
		   (* (rational-denominator x) (rational-denominator y))))

(defun rational-sub (x y)
  (rational-create (- (* (rational-numerator x) (rational-denominator y))
		      (* (rational-numerator y) (rational-denominator x)))
		   (* (rational-denominator x) (rational-denominator y))))

(defun rational-mul (x y)
  (rational-create (* (rational-numerator x) (rational-numerator y))
		   (* (rational-denominator x) (rational-denominator y))))

(defun rational-div (x y)
  (rational-create (* (rational-numerator x) (rational-denominator y))
		   (* (rational-numerator y) (rational-denominator x))))

(defun rational-equal? (x y)
  (= (* (rational-numerator x) (rational-denominator y))
     (* (rational-denominator x) (rational-numerator y))))
