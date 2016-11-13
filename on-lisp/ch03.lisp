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
