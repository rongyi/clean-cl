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

;; sort's first arg is a list
(sort '(1 2 3 9 8 7) #'>)
;; remove-if equal to reject
(remove-if #'evenp '(1 3 5 7))
;; remove-if-not equals to filter
(remove-if-not #'evenp '(1 3 5 7 8))
(defun ry/remove-if (fn lst)
  (if (null lst)
      nil
      (if (funcall fn (car lst))
          (ry/remove-if fn (cdr lst))
          (cons (car lst)
                (ry/remove-if fn (cdr lst))))))

;; (ry/remove-if #'evenp '(1 3 4 5))

(defun ry/remove-if-not (fn lst)
  (ry/remove-if #'(lambda (x)
                    (not (funcall fn x)))
                lst))

;; (ry/remove-if-not #'evenp '(1 3 5 7 8))
;; closure test
;; lexical scope is the default in Common Lisp, so the system must save
;; copies of the bindings of those variables at the time the function
;; was defined. Such a combination  of a function and a set of varialbe
;; bindings is called a /closure/

(let ((y 'defined))
  (defun scope-test (x)
    (list x y)))

(let ((y 'dynamic))
  (scope-test 7))

(defun list+ (lst n)
  (mapcar #'(lambda (x) (+ x n))
          lst))

;; (list+ '(1 2 3) 10)
;; using closure
(let ((count 0))
  (defun genid ()
    (incf count))
  (defun reset-id ()
    (setq count 0)))

;; (genid)
;; (reset-id)

(defun make-adder (n)
  #'(lambda (x) (+ x n)))

;; why we need funcall?
(let ((add10 (make-adder 10)))
  (funcall add10 99))
(setf add10 (make-adder 10))
(funcall add10 99)

;; closure's state can be changed
(defun make-adder (n)
  #'(lambda (x &optional change)
      (if change
          (progn
            (setq n x)
            (format t "reset value~%"))
          (+ x n))))

(setq addchange (make-adder 1))
(funcall addchange 3)
(funcall addchange 10 t)
(format t "hell world~%")
