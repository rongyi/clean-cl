(defmacro ry/for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
        (limit ,stop))
       ((> ,var limit))
     ,@body))

;; (macroexpand-1 '(ry/for (limit 1 5)
;;                  (princ limit)))

;; variable capture
