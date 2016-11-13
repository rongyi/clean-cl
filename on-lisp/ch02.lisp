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

(defmacro defalias (nickname realname)
  `(setf (symbol-function ',nickname) #',realname))

;; test push and delete
(setf lst '(a b c))
(push 'd lst)
(delete 'd lst)
;; now a test
(defun make-dbms (db)
  (list
   #'(lambda (key)
       (cdr (assoc key db)))
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   #'(lambda (key)
       (setf db (delete key db :key #'car))
       key)))
(setq cities (make-dbms '((boston . us) (prais . france))))
(funcall (car cities) 'boston)
(funcall (second cities) 'london 'england)
(funcall (car cities) 'london)
(defun lookup (key db)
  (funcall (car db) key))
(lookup 'london cities)


;; (funcall #'push 'd lst)
;; (macroexpand-1 '(defalias insert push))
;; (defalias insert push)

;; local function
;; we can't use lambda to define a recursive function,
;; because the function will have no way of refering to itself.
;; define function
(labels ((inc (x) (1+ x)))
  (inc 3))
(defun ry/count-instances (obj lsts)
  (labels ((instance-in (lst)
             (if (consp lst)
                 (+ (if (eq (car lst) obj)
                        1
                        0)
                    (instance-in (cdr lst)))
                 0)))
    (reduce #'+ (mapcar #'instance-in lsts))))

(ry/count-instances 'a '('a b c))
(reduce #'+ '(1 2 3) :initial-value 10)


;; tail recursion
;; this is not tail recursion
(defun ry/length (lst)
  (if (null lst)
      0
      (1+ (ry/length (cdr lst)))))
;; (ry/length '(1 2 3))

;; this *is* tail recursion
(defun ry/find-if (fn lst)
  (if (funcall fn (car lst))
      (car lst)
      (ry/find-if fn (cdr lst))))

;; (ry/find-if #'evenp '(1 2 3))

(defun ry/length (lst)
  (labels ((rec (lst acc)
             (if (null lst)
                 acc
                 (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))

;; (ry/length '(1 2 3))

;; force compiler to expand tail recursion to loop
;; (proclaim '(optimize speed))


;; compilation
(defun foo (x)
  (1+ x))
;; check compile
(compiled-function-p #'foo)
;; compile action, format
(compile 'foo)
(compile nil '(lambda (x)
               (+ x 2)))
;; a bit ugly
(progn (compile 'bar '(lambda (x) (* x 2)))
       (compiled-function-p #'bar))

;; inline a function
(defun 50th (lst)
  (nth 49 lst))
(proclaim '(inline 50th))
(defun foo (lst)
  (+ (50th lst) 1))
