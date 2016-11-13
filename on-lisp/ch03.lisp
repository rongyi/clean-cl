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
