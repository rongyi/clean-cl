(defun test (x &key y z)
  (list x y z))

;; error!
;; (test 'a 'b)
;; (test 'a :y 'b)

(defun test2 (x &rest args)
  (apply x args))

(test2 '+ 1 2 3 4)
;; optional with default

(defun test3 (x &optional (y 'c))
  (list x y))
;; (test3 'a 'b)
;; (test3 'a)

(defun single? (x)
  (and (consp x) (null (cdr x))))

;; (single? '(x))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

;; (map-int 'identity 10)
;; (let ((x '(1 2 3)))
;;   (push 4 x))

(defun ry/filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push x acc))))
    (nreverse acc)))

;; (ry/filter 'oddp '(1 2 3 4 5))

(defun ry/most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (x (cdr lst))
          (let ((val (funcall fn x)))
            (if (> val max)
                (setf wins x
                      max val))))
        (values wins max))))

;; (ry/most 'length '((a b c d) (a b)))

;; (destructuring-bind (fn1 . rest) '(a b c d e)
;;   (values fn1 rest))

;; (reduce '(lambda (a b)
;;           ()))

;; (reduce '+ '(1 2 3 4) :initial-value 10)

;; (reduce 'list '(1 2 3 4))


(defun compose (&rest args)
  (destructuring-bind (fn1 . rest) (reverse args)
    (lambda (&rest args)
      (reduce (lambda (v f)
                (funcall f v))
              rest
              :initial-value (apply fn1 args)))))

;; (mapcar (compose 'list 'round 'sqrt)
;;         '(4 9 16 25))
