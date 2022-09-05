(defun sum (list)
  "Returns the sum of all the values."
  (reduce #'+ list))

(defun average (list)
  "Returns the average, which is the sum of the values divided by the number of values there is"
  (/ (sum list) (length list)))

(defun median (list)
  "Returns the value sequentially in the middle of the list. If the list has an even number of values, the median is the mean of the two middle values"
  (flet ((l-r (x);For the even branch, if we get a number like 7.5, we want the index 7 and 8
	   (let ((temp (1- (+ 1/2 (/ x 2)))));We remove 1 because the index starts at 0
	     (list (floor temp) (ceiling temp)))))
    (let ((length (length list)))
      (if (evenp length)
	  (/ (+ (elt list (car (l-r length))) (elt list (cadr (l-r length)))) 2);We find the middle, which is a half number, and then get the average of the values on the left and right of it
	  (elt list (1- (+ 1/2 (/ length 2))))))));The number we want is at 1/2x +1/2, we then substract by 1 because the car of a list is at index 0

(defun occurences (list)
  "Returns the frequencies of elements in a list"
  (let ((test-list nil))
    (flet ((counter (x)
	     (if (assoc x test-list)
		 (incf (cadr (assoc x test-list)))
		 (push (list x 1) test-list))))
      (mapcar #'counter list)
      test-list)))

(defun mode (list)
  "Returns the most frequent element and the number of times it occured"
  (reduce (lambda (x y) (if (> (cadr x) (cadr y)) x y)) (occurences list)))
