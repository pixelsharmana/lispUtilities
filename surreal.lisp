(load "/home/pixel/prog/lispUtilities/unitTest.lisp")
(load "/home/pixel/prog/lispUtilities/math/surreal.lisp")

(deftest test0-and-1 ()
  (check
    (sur-less-or-equal sur-zero sur-one)
    (not (sur-less-or-equal sur-one sur-zero))))

(deftest test0-and-minus-1 ()
  (check
    (not (sur-less-or-equal sur-zero sur-minus-one))
    (sur-less-or-equal sur-minus-one sur-zero)))

(deftest test-minus1-and-1 ()
  (check
    (not (sur-less-or-equal sur-one sur-minus-one))
    (sur-less-or-equal sur-minus-one sur-one)))

(deftest test-two ()
  (check
    (sur-less-or-equal sur-zero sur-two)
    (sur-less-or-equal sur-minus-one sur-two)
    (not (sur-less-or-equal sur-two sur-zero))
    (not (sur-less-or-equal sur-two sur-one))
    (not (sur-less-or-equal sur-two sur-minus-one))))

(deftest test-naturals ()
  (check
    (test0-and-1)
    (test0-and-minus-1)
    (test-minus1-and-1)
    (test-two)))

(deftest test-half ()
  (check
    (sur-less-or-equal sur-zero sur-half)
    (not (sur-less-or-equal sur-half sur-zero))
    (not (sur-less-or-equal sur-one sur-half))
    (sur-less-or-equal sur-half sur-two)
    (sur-less-or-equal sur-minus-one sur-half)
    (not (sur-less-or-equal sur-half sur-minus-one))))

(deftest test-all ()
  (check
    (test-naturals)
    (test-half)))
;This one is mostly just for fun

(defun pair (left right)
  (list left right))

(defun succ (surreal)
  (pair surreal nil))

(defparameter sur-zero 
  (pair nil nil))

(defparameter sur-one
  (succ sur-zero))

(defparameter sur-minus-one
  (pair nil sur-zero))

(defparameter sur-half
  (pair sur-zero sur-one))

(defparameter sur-two
  (succ sur-one))

(defparameter sur-minus-two
  (pair nil sur-minus-one))

(defparameter sur-three-quarters
  (pair sur-half sur-one))

(defparameter tt
  (succ sur-half))

(defparameter ty
  (pair sur-one sur-two))

(defparameter p0
  (pair sur-one sur-two))

(defparameter p1
  (pair sur-one (succ sur-two)))

(defparameter p2
  (pair sur-one (succ (succ sur-two))))

(defparameter p3
  (pair sur-one (succ (succ (succ sur-two)))))

(defun sur-less-or-equal (object1 object2)
  (unless (or (null object1) (null object2))
      (and (not (or (eq (first object1) object2) (sur-less-or-equal object2 (first object1))))
	   (not (or (eq (second object2) object1) (sur-less-or-equal (second object2) object1))))))

(defun sur-print (surreal)
  (format t "<~a | ~a>~%" (first surreal) (second surreal))
  t)


;(defun sur-less-or-equal (number1 number2)
  
