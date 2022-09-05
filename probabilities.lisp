(defun bayes (prob-of-a prob-of-b-given-a prob-of-b-given-not-a)
  "Calculates the probability of a given b. All values are between 0 and 1"
  (/ (* prob-of-b-given-a prob-of-a)
     (+ (* prob-of-b-given-a prob-of-a)
	(* prob-of-b-given-not-a (- 1 prob-of-a)))))
