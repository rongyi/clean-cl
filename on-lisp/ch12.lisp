;; (setq lst '(a b c))
;; (setf (car lst) 480)

;; (progn (rplaca lst 480) 480)
;; This transformation from query to assertion is called *inversion*
;; All the most frequently used Common Lisp access functions have predefined inversions,
;; including car, cdr, nth, aref, get, gethash, and the access functions created by defstruct


;; An expression which can serve as the first arguments to setf is called a /generlized variable/.
;; generlized variable have turned out to be a powerful abstraction.


;; Wrong version
(defmacro toggle (obj)
  `(setf ,obj (not ,obj)))

(let ((lst '(a b c)))
  (toggle (car lst))
  lst)
(testmacro (toggle (car '(a b c))))

;; Generalized variable are like a health food that tastes good.
;; The yield programs which are virtuously modular, and yet beautifully elegant.

;; (toggle (nth (incf 0) lst))
;; This increments i twice, and sets the (i + 1)th element to the opposite of the
;; (i + 2)th element.
(let ((lst '(t nil t))
      (i -1))
  (toggle (nth (incf i) lst))
  lst)

(define-modify-macro toggle () not)
(let ((lst '(t nil t))
      (i -1))
  (toggle (nth (incf i) lst))
  lst)

(defmacro allf (val &rest args)
  (with-gensyms (gval)
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a)
                           (list a gval))
                       args)))))

(defmacro nilf (&rest args)
  `(allf nil ,@args))
(defmacro tf (&rest args)
  `(allf t ,@args))

(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a)
                   `(toggle2 ,a))
               args)))

(define-modify-macro toggle2 () not)
;; mapcan can be see as a glue
;; (mapcan #'(lambda (x)
;;             (if (oddp x)
;;                 (list x)))
;;         '(1 2 3 4 5))
