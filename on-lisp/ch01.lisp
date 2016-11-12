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

(setq x #'append)
(eq (symbol-value 'x)
    (symbol-function 'append))

;; forget what does append do
;; (append '(a b) '(b c))


;; apply need a list
;; (+ 1 2)
;; (apply '+ '(1 2))
;; funcall need scattered element
;; (funcall '+ 1 2 3)

(apply (symbol-function '+) '(1 2))
(apply #'(lambda (x y)
           (+ x y))
       '(1 2))

(apply #'+ 1 '(2))

;; mapcar's first arg is fn
(mapcar #'(lambda (x)
            (- x 10))
        '(1 2 3))

;; get the min size of two list
(mapcar #'+
        '(1 2 3)
        '(10 20 30 40))
