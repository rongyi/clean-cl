;; In writing macros like for, one must remember that the arguments to a macro are forms, not values.

(defmacro ry/for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

;; multiple evaluations:
(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var)))
       ((> ,var ,stop))
     ,@body))

;; order bug would be extremely difficult to find.

;; (defmacro crazy (expr)
;;   (nconc expr (list t)))
;; (defun foo()
;;   (crazy (list)))
;; (foo)

;; The upshot is, don't try to avoid consing by destructively modifying parameter list structure.

(defun nth-fn (n lst)
  (if (= n 0)
      (car lst)
      (nth-fn (- n 1)
              (cdr lst))))

(defmacro nthd (n lst)
  `(nth-fn ,n ,lst))

(defmacro nthe (n lst)
  `(labels ((nth-fn (n lst)
              (if (= n 0)
                  (car lst)
                  (nth-fn (- n 1)
                          (cdr lst)))))
     (nth-fn ,n ,lst)))

(defun or-expand (args)
  (if (null args)
      nil
      (let ((sym (gensym)))
        `(let ((,sym ,(car args)))
           (if ,sym
               ,sym
               ,(or-expand (cdr args)))))))

(defmacro ora (&rest args)
  (or-expand args))
(defmacro orb (&rest args)
  (if (null args)
      nil
      (let ((sym (gensym)))
        `(let ((,sym ,(car args)))
           (if ,sym
               ,sym
               (orb ,@(cdr args)))))))

;; (ora 'nil 2)
;; (macroexpand-1 (ora '(3 2)))
;; (orb 'nil 2)
