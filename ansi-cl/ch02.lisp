(format t "Hello ~A plus ~A equals ~A ~%" 2 3 (+ 2 3))

(read)
(defun ask-number ()
  (format t "please input a number. ~%")
  (let ((val (read)))
    (if (numberp val)
        val
        (ask-number))))

(defparameter *glob* 99)
(defconstant limit (* *glob* 99))
(boundp '*glob*)
(setf *glob* 98)
(let ((n 10))
  (setf n 2)
  n)
(setf x (list 'a 'b 'c))
(setf (car x) 'x)
(setf lst '(c a r a t))
(remove 'a lst)

;; end inclusive
(defun show-square (start end)
  (do ((i start (+ i 1)))
      ((> i end))
    (format t "~A ~A ~%" i (* i i))))

(dolist (i '(1 2 3 4 5))
  (format t "~A " i))

(apply '+ '(1 2 3))
(apply '+ 1 2 3 '(4 5))
(funcall '+ 1 2 3 4 5)
(typep 17 'integer)
(type 17)
