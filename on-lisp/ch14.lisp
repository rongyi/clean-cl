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

(defun count-instance (obj lists)
  (mapcar (alambda (lst)
            (if lst
                (+ (if (eq (car lst) obj) 1 0)
                   (self (cdr lst)))
                0))
          lists))

;; (count-instance 'a '((a b c d) (a a a a)))

(defmacro ablock (tag &rest args)
  `(block ,tag
     ,(funcall (alambda (args)
                 (case (length args)
                   (0 nil)
                   (1 (car args))
                   (t `(let ((it ,(car args)))
                         ,(self (cdr args))))))
               args)))

;; (ablock t
;;         (princ "ho ")
;;         (princ it)
;;         (princ it)
;;         (return-from t))

;; In Common Lisp the symbol nil has at least three different jobs:
;; 1. empty list
;; (cdr '(a))
;; 2. represent falsity
;; (= 1 0)
;; 3. function return nil to indicate failure
;; (find-if #'oddp '(2 4 6))

;; return assoc
;; (setq synonyms '((yes . t) (no . nil)))
;; (assoc 'no synonyms)

;; return list
;; (member-if #'null '(2 nil 6))

;; return multiple values
;; (setf edible (make-hash-table)
;;       (gethash 'olive-oil edible) t
;;       (gethash 'motor-oil edible) nil)
;; (gethash 'motor-oil edible)

(defun edible? (x)
  (multiple-value-bind (val find?) (gethash x edible)
    (if find?
        (if val 'yes 'no)
        'maybe)))
;; (mapcar #'edible? '(motor-oil olive-oil iguana))
