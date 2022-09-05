(load "unitTest.lisp")

(deftest test+- ()
  (check
    (= (+ 2 3 5) 10)
    (= (+ 2 3 7) 12)
    (= (+ 1 0 1) 2)
    (= (+ 4 4 8) 16)
    (= (+ 6 6 6) 18)))

(deftest test* ()
  (check
    (= (* 2 3) 6)
    (= (* 2 7) 14)
    (= (* 1 0 1) 0)
    (= (* 2 8) 16)
    (= (* 6 6) 36)))

(deftest test-arithemetic ()
  (check
    (test+-)
    (test*)))
