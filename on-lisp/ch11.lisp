(defmacro ry/let (binds &body body)
  `((lambda ,(mapcar #'(lambda (x)
                         (if (consp x)
                             (car x)
                             x))
                     binds)
      ,@body)
    ,@(mapcar #'(lambda (x)
                  (if (consp x)
                      (cadr x)
                      nil))
              binds)))

;; (ry/let ((x 1)
;;          (y 2))
;;   (+ x y))

(defmacro testmacro (expr)
  `(pprint (macroexpand-1 ',expr)))

;; (testmacro (ry/let ((x 1)
;;                     (y 2))
;;              (+ x y)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro when-bind* (binds &body body)
  (if (null binds)
      `(progn ,@body)
      `(let (,(car binds))
         (if ,(caar binds)
             (when-bind* ,(cdr binds) ,@body)))))

;; (when-bind* ((x (find-if #'consp '(a (1 2) b)))
;;              (y (find-if #'oddp x)))
;;   (+ y 10))
;; (find-if #'oddp '(2 4 1))

(defmacro ry/with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))



(defun condlet-clause (vars cl bodfn)
  `(,(car cl) (let ,(mapcar #'cdr vars)
                (let ,(condlet-binds vars cl)
                  (,bodfn ,@(mapcar #'cdr vars))))))

(defun condlet-binds (vars cl)
  (mapcar #'(lambda (bindform)
              (if (consp bindform)
                  (cons (cdr (assoc (car bindform) vars))
                        (cdr bindform))))
          (cdr cl)))

(defmacro condlet (clauses &body body)
  (let ((bodfn (gensym))
        (vars (mapcar #'(lambda (v)
                          (cons v (gensym)))
                      (remove-duplicates
                       (mapcar #'car
                               (mappend #'cdr clauses))))))
    `(labels ((,bodfn ,(mapcar #'car vars)
                ,@body))
       (cond ,@(mapcar #'(lambda (cl)
                           (condlet-clause vars cl bodfn))
                       clauses)))))

(condlet (((= 1 2) (x (princ 'a)) (y (princ 'b)))
          ((= 1 1) (x (princ 'c)) (y (princ 'd)))
          (t (x (princ 'e)) (z (princ 'f))))
  (list x y z))
