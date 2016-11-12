;; lisp is a programmable  programming language
;; in lisp programs are data
(defun double (x)
  "double X"
  (* 2 x))
;; (double 2)
;; (eq #'double (car (list #'double)))

((lambda (x)
   (* x 2)) 3)

;; different namespace for varialbe and function
(setf double 2)
(double double)

;; to get a varialble from symbole namespace
(symbol-value 'double)
;; to get a var from function namespace
(symbol-function 'double)
