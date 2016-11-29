(defmacro ry/for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
       ((> ,var limit))
     ,@body))

;; (macroexpand-1 '(ry/for (limit 1 5)
;;                  (princ limit)))

;; variable capture
;; Free: A symbol s occurs free in an expression when it is used as a variable in that
;; expression, but the expression does not create a binding for it.

;; example
;; (let ((x y) (z 10))
;;   (list w x z))

;; Skeleton: The skeleton of a macro expansion is the whole expansion, minus anything which was part of an
;; argument in the macro call

;; Capturable: A symbol is capturable in some macro expansion if:
;; a. it occurs free in the skeleton of the macro expansion
;; b. it is bound by a part of the skeleton in which argument passed to
;; the macro are either bound or evaluated.

(defmacro ry/for ((var start stop) &body body)
  `(do ((b #'(lambda (,var)
               ,@body))
        (count ,start (1+ count))
        (limit ,stop))
       ((> count limit))
     (funcall b count)))

;; using gensym
(defmacro ry/for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))
