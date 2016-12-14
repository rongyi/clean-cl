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

;; we can't express a recursive function with a simple lambda-expression.
(defun count-instance (obj lists)
  (labels ((instances-in (lst)
             (if lst
                 (+ (if (eq (car lst) obj) 1 0)
                    (instances-in (cdr lst)))
                 0)))
    (mapcar #'instances-in lists)))

;; (count-instance 'a '((a b c) (a b c d e) (a a a a a)))

(defmacro alambda (parms &body body)
  `(labels ((self ,parms ,@body))
     #'self))

;; (funcall (alambda (x) (if (= x 0) 1 (* x (self (1- x)))))
;;           3)
