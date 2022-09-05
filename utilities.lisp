(defun flatten (structure)
  "Transforms any arbitrarily nested list into a flat list"
  (cond ((null structure) nil)
	((atom structure) (list structure))
	(t (mapcan #'flatten structure))))

(defun tack (place obj)
  "Tacks place at the end of obj, returns a proper list"
  (if (and
       (not (listp place))
       (not (listp obj)))
      (list place obj)
      (if (not (listp place))
	  (append (list place) obj)
	  (append place (cons obj nil)))))

(defmacro ntack (place obj)
  "Destructive version of tack"
  `(setf ,place (tack ,place ,obj)))

(defun last1 (sequence)
  "Returns the last element of a sequence"
  (car (last sequence)))

(defun add-margin (box margin)
  "Add some margin to a list of 4 elements, being the x, y, width and height of the box"
  (let ((half-margin (/ margin 2)))
    (list
     (- (elt box 0) half-margin)
     (- (elt box 1) half-margin)
     (+ (elt box 2) margin)
     (+ (elt box 3) margin))))

;(defun range (length)
;  (if (= length 0)
;      0
;      (tack (range (- length 1)) length)))

(defun range (length)
  (declare (fixnum length))
  (loop for i upto (- length 1) nconc (list i)))

(defun shuffle (list)
  "Returns a random permutation of list"
  (let ((temp (copy-tree list)))
    (loop for i from (length temp) downto 2
	  do (rotatef (elt temp (random i))
		      (elt temp (1- i))))
    temp))

;(defun get-unicode (x)
;  (concatenate 'string #\  (write-to-string x)))
;use char-code instead? Read on that

(defun to-hex (number)
  (cond ((= number 0) "0")
	((= number 1) "1")
	((= number 2) "2")
	((= number 3) "3")
	((= number 4) "4")
	((= number 5) "5")
	((= number 6) "6")
	((= number 7) "7")
	((= number 8) "8")
	((= number 9) "9")
	((= number 10) "A")
	((= number 11) "B")
	((= number 12) "C")
	((= number 13) "D")
	((= number 14) "E")
	((= number 15) "F")
	(t number)))

(defun create-uuid ()
  (defun make-hex-string (length)
    (let ((result nil))
      (dotimes (i length)
	(setf result (concatenate 'string result (to-hex (random 16)))))
      (return-from make-hex-string result)))
  ;(read-from-string ;to make into a symbol
   (concatenate 'string
		(make-hex-string 8)
		"-"
		(make-hex-string 4)
		"-4"
		(make-hex-string 3)
		"-"
		(elt (assoc (random 4)
			    '((0 "8")
			      (1 "9")
			      (2 "A")
			      (3 "B"))) 1)
		(make-hex-string 3)
		"-"
		(make-hex-string 12)))

(defun lerp (value-0 value-1 percentage)
  (+ value-0 (* percentage (- value-1 value-0))))

(defun change-range (input initial-range-start initial-range-end final-range-start final-range-end)
  (+ final-range-start
     (* (- input initial-range-start)
	(/ (- final-range-end final-range-start)
	   (- initial-range-end initial-range-start)))))

(defun random-between (range-start range-end)
  (change-range (random 1.0) 0 1 range-start range-end))

(defun memoize (fun)
  "Returns a memoized version of fun, in which inputs have their outputs cached for faster retrival of already-computed results"
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	      (setf (gethash args cache)
		    (apply fun args)))))))

(defun sort-copy (sequence predicate)
  (sort (copy-seq sequence) predicate))

(defun enlist (item)
  "Transforms item into a list if it isn't one already"
  (if (listp item)
      item
      (list item)))

(defun elt-bound (sequence index)
  "Returns nil if elt is out-of-bounds"
  (cond ((> 0 index) nil)
	((< (length sequence) index) nil)
	(t (elt sequence index))))

(defun elt-random (sequence)
  "Selects a random element of sequence"
  (elt-bound sequence (random (length sequence))))

(defun but-last (sequence)
  "Returns sequence without its last element"
  (subseq sequence 0 (1- (length sequence))))

(defun index-fun (function sequence)
  "Reduces the sequence using the function and gives the index of the result in the sequence"
  (position (reduce function sequence) sequence))

(defun max-index (sequence)
  "Finds the index of the element with the highest value"
  (index-fun #'max sequence))

(defun min-index (sequence)
  "Finds the index of the element with the lowest value"
  (index-fun #'min sequence))

(defun between (number min-bound max-bound)
  (when (and (> number min-bound) (< number max-bound))
    number))

(defun wrap (number min-bound max-bound)
  (if (between number min-bound max-bound)
      number
      (+ (mod number max-bound) min-bound)))

(defun get-moore-neighbors (x y)
  (list
   (list (- x 1) (- y 1)) (list (- x 1) y) (list (- x 1) (+ y 1))
   (list x (- y 1)) (list x (+ y 1))
   (list (+ x 1) (- y 1)) (list (+ x 1) y) (list (+ x 1) (+ y 1))))

(defun hsv-to-rgb (input)
  "Takes a list of 3 values, corresponding respectively to the Hue, saturation and value of a colour, and transforms it into Red, Green and Blue values. The hue must be between 0 and 360, the saturation and value must be between 0.0 and 1.0"
  (let* (
	 (c (* (elt input 2) (elt input 1)))
	 (x (* c (- 1 (abs (- (mod (/ (elt input 0) 60) 2) 1)))))
	 (m (- (elt input 2) c)))
    (let ((temp
	    (cond ((and (>= (elt input 0) 0) (< (elt input 0) 60)) (list c x 0))
		  ((and (>= (elt input 0) 60) (< (elt input 0) 120)) (list x c 0))
		  ((and (>= (elt input 0) 120) (< (elt input 0) 180)) (list 0 c x))
		  ((and (>= (elt input 0) 180) (< (elt input 0) 240)) (list 0 x c))
		  ((and (>= (elt input 0) 240) (< (elt input 0) 300)) (list x 0 c))
		  ((and (>= (elt input 0) 300) (< (elt input 0) 360)) (list c 0 x)))))
      (list (* (+ (elt temp 0) m) 255)
	    (* (+ (elt temp 1) m) 255)
	    (* (+ (elt temp 2) m) 255)))))

;(defun rgb-to-hsv (input)

(defun elt-change (sequence index value)
  "Non-destructively changes an element in a sequence"
  (let ((temp (copy-tree sequence)))
    (append (tack (subseq temp 0 index) value) (subseq temp (+ 1 index)))))

(defun elt-insert (sequence index value)
  "Non-destructively inserts an element in a sequence"
  (let ((temp (copy-tree sequence)) (true-index index))
    (when (< true-index 0)
      (setf true-index 0))
    (when (> true-index (length sequence))
      (setf true-index (length sequence)))	     
    (append (tack (subseq temp 0 true-index) value) (subseq temp true-index))))

(defun elt-remove (sequence index)
  "Non-destructively removes an element in a sequence"
  (let ((temp (copy-tree sequence)))
    (append (subseq temp 0 index) (subseq temp (+ 1 index)))))

(defun keep-based-on (sequence1 sequence2)
  (map 'list (lambda (a b) (unless (null b) a)) sequence1 sequence2))

(defun keep-if-index (predicate sequence)
  (remove-if #'null (keep-based-on sequence (mapcar predicate (range (length sequence))))))

(defun mapcar-plist (function list)
  "Applies a function over a plist, skipping over the keywords"
  (mapcar (lambda (i) (if (keywordp i) (identity i) (funcall function i))) list))

(defun number-length (number)
  "Number of digits in the supplied number"
  (ceiling (log number 10)))

;defun reverse of explose

(defun explode (number)
  "Creates a list of the digits in a number, little-endian"
  (defun explode-helper (number index)
    (truncate (/ (mod number (expt 10 (+ index 1))) (expt 10 index))))
  (mapcar (lambda (i) (explode-helper number i)) (range (number-length number))))

(defun concatenate-numbers (a b)
  (+ (* a (expt 10 (number-length b))) b))

(defun de-explode (list)
  "Create a number from a little-endian list of numbers"
  (reduce (lambda (a b) (concatenate-numbers b a)) list))

(defun binary-list (number &optional acc)
  "Give the bits of a number in big-endian"
  (cond ((zerop number) (or acc (list 0)))
	((plusp number) (binary-list (ash number -1) (cons (logand 1 number) acc)))
	(t (error "~S: non-negative argument required, got ~s" 'binary-list number))))

(defun binary-list-to-decimal (list)
  "Transforms a list of binary numbers into its decimal representation"
  (reduce (lambda (x y) (+ (* 2 x) y)) list))

(defun save-file (data filename)
  "Saves the Lisp expression 'data' to the path 'filename'"
  (with-open-file (out filename
		       :direction :output
		       :if-exists :supersede)
    (with-standard-io-syntax
      (format out "~a" data))))

(defun load-file (filename)
  "Loads the lisp expression from the file at 'filename'"
  (with-open-file (in filename)
    (with-standard-io-syntax
      (read in))))

(defun files-in-directory (directory)
  (directory (concatenate 'string directory
			  (unless (eq #\/ (elt directory (1- (length directory))))
			    "/")
			  "*.*")))

(defun append-to-file (data filename)
  (with-open-file (out filename :direction :output
				:if-exists :append
				:if-does-not-exist :create)
    "Opens a file to the path 'filaname' and append something to it"
    (format out "~a~%" data)))
