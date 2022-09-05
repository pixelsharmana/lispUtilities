(defun trans-function (symbol)
  "Takes either a function or a symbol, tries to transform a symbol into a function"
  (if (functionp symbol)
      symbol
      (fdefinition symbol)))

(defun compose (f g)
  "Produces a function whose output is the composition of the function g on f"
  (lambda (args) (funcall g (funcall f args))))

(defun composef (f g)
  "Produces a function whose output is the composition of the function g on f, accepts symbols referring to functions too"
  (compose (trans-function f) (trans-function g)))

(defun register-function (name function)
  "Assigns the function function to the global function name name"
  (setf (symbol-function name) function))

(defun iterate (n f)
  "Produces a function whose output is the repeated application of the function f, n times"
  (if (= n 0)
      (identity f)
      (compose f (iterate (max 0 (- n 1)) f))))

(defun parallel-combine (h f g)
  (lambda (args) (funcall h (apply f args) (apply g args))))

(defun triple (x)
  (* x 3))

(defun half (x)
  (/ x 2))

(defun square (x)
  (* x x))
