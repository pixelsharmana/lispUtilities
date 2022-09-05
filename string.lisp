(load "utilities.lisp")

(defun string-num-to-char (number)
  "Transforms a number into a char. Only the most significant digit is used. Returns nil if not a number"
  (when (numberp number)
    (elt (format nil "~a" number) 0)))

(defun string-symbol-to-string (symbol)
  "Transforms a symbol to a string representation"
  (when (symbolp symbol)
    (string symbol)))

(defun string-to-char (string)
  "Returns the first element of a string, also accepts chars, numbers and symbols"
  ;From most expected to least expected
  (cond	((stringp string) (elt string 0))
	((characterp string) string);Already a char? Return it
	((numberp string) (string-num-to-char string));If it's a number, transform the most significant digit into a char
	((symbolp string) (elt (string string) 0));Sure, pass a symbol, who cares...
	(t nil)));Anything else is Nil

(defun string-find-char (char string &optional (start 0))
  "Return the index of the first occurence of char in string"
  (let ((search-char (string-to-char char)))
    (position search-char string :start start)))

(defun string-find-all-chars (char string)
  "Returns a list of the index of every occurence of char in string"
  (let ((search-char (string-to-char char))
	(list nil))
    (dotimes (i (length string))
      (when (string= search-char (elt string i))
	(ntack list i)))
    list))

(defun string-to-chars (string)
  "Returns a list of all the chars composing the string"
  (let ((list nil))
    (dotimes (i (length string))
      (ntack list (elt string i)))
    list))
