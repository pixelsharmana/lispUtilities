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
