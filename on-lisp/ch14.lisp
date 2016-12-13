;; capture
;; a means anaphoric,代称,代指,这里指变量capture
(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

;; (aif t
;;      "hell"
;;      "mail")

(defmacro awhen (test-form &body body)
  `(aif ,test-form
        (progn ,@body)))

;; (awhen t
;;   (princ "hell")
;;   (princ "world"))

(defmacro awhile (expr &body body)
  `(do ((it ,expr ,expr))
       ((not it))
     ,@body))

(defmacro aand (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(aif ,(car args) (aand ,@(cdr args))))))

;; (aand t t nil)

(defmacro acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses))
            (sym (gensym)))
        `(let ((,sym ,(car cl1)))
           (if ,sym
               (let ((it ,sym))
                 ,@(cdr cl1))
               (acond ,@(cdr clauses)))))))
