(do ((i 0 (1+ i)))
    ((>= i 3))
  (format t "hello~%"))

(defun good-reverse (lst)
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

;; (good-reverse '(1 2 3))
(truncate 3.1415926)
(= (truncate 3.14) 3)

;; bind multiple value at once
(multiple-value-bind (int frac) (truncate 3.14)
  (list int frac))

;; return more than one valus
(defun power (x)
  (values x (sqrt x) (expt x 2)))

(multiple-value-bind (base root square) (power 4)
  (list base root square))

;; side effect rule should be: a given invocation can safely modify what it uniquely owns
;; If you call it twice with the same arguments, you should get the same results.

;; Incremental testing is so valuable that Lisp style has evolved to take advantage of it.

;; Lisp programmer actually design their programs to be easy to test:
;; 1. They try to segreate side effect in a few functions, allowing the greater
;; part of the program to be written in a purely functional style
;; 2. If a function must perform side-effects, they try at least to give it a functional interface.
;; 3. They give each function a single, well-defined purpose.
