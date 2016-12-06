;; (setq lst '(a b c))
;; (setf (car lst) 480)

;; (progn (rplaca lst 480) 480)
;; This transformation from query to assertion is called *inversion*
;; All the most frequently used Common Lisp access functions have predefined inversions,
;; including car, cdr, nth, aref, get, gethash, and the access functions created by defstruct


;; An expression which can serve as the first arguments to setf is called a /generlized variable/.
;; generlized variable have turned out to be a powerful abstraction.


;; Wrong version
;; (defmacro toggle (obj)
;;   `(setf ,obj (not ,obj)))

;; (let ((lst '(a b c)))
;;   (toggle (car lst))
;;   lst)

;; Generalized variable are like a health food that tastes good.
;; The yield programs which are virtuously modular, and yet beautifully elegant.
