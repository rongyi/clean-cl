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

(testmacro (condlet
               (((= 1 2) (x (princ 'a)) (y (princ 'b)))
                ((= 1 1) (x (princ 'c)) (y (princ 'd)))
                (t (x (princ 'e)) (z (princ 'f))))
             (list x y z)))

;; 有点懵逼，这后面全是解释
;; test code to understand the code above

;; the code to generate parameter
(defun test (clauses &rest rest)
  (remove-duplicates (mapcar #'car (mappend #'cdr clauses))))
(test '(((= 1 2) (x (princ 'a)) (y (princ 'b)))
        ((= 1 1) (x (princ 'c)) (y (princ 'd)))
        (t (x (princ 'e)) (z (princ 'f)))))
(cdr  (assoc 'x '((x 1) (y 2))))

;; sample output

;; (LABELS ((#:G4002 (Y X Z) (LIST X Y Z)))
;;  (COND
;;   ((= 1 2)
;;    (LET (#:G4003 #:G4004 #:G4005)
;;           ___________________ ==> this is generated by condlet-binds
;;     (LET ((#:G4004 (PRINC 'A)) (#:G4003 (PRINC 'B)))
;;      (#:G4002 #:G4003 #:G4004 #:G4005))))
;;   ((= 1 1)
;;    (LET (#:G4003 #:G4004 #:G4005)
;;     (LET ((#:G4004 (PRINC 'C)) (#:G4003 (PRINC 'D)))
;;      _________________________________ ==> (,bodfn ,@(mapcar #'cdr vars))
;;      (#:G4002 #:G4003 #:G4004 #:G4005))))
;;   (T
;;    (LET (#:G4003 #:G4004 #:G4005)
;;     (LET ((#:G4004 (PRINC 'E)) (#:G4005 (PRINC 'F)))
;;      (#:G4002 #:G4003 #:G4004 #:G4005))))))

;; The names of context building macros often begin with with-.

;; (setq x 'a)
;; (unwind-protect
;;      (progn (princ "what error?")
;;             (error "this error"))
;;   (setq x 'b))


(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
     ((nil) ,nil-case)
     (? ,?nil-case)
     (t ,t-case)))

(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) , zero)
             (t ,neg)))))

;; (mapcar #'(lambda (x)
;;             (nif x 'p 'z 'n))
;;         '(0 1 -1))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c)
                         `(eql ,insym ,c))
                     choices)))))
;; (in t nil nil nil t)
;; (in t nil nil nil nil)

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a)
                          `',a)
                      args)))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
                         `(funcall ,fnsym ,c))
                     choices)))))


(defun >casex (g cl)
  (let ((key (car cl))
        (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))

(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (cl)
                           (>casex g cl))
                       clauses)))))

;; iteration
(defmacro forever (&body body)
  `(do ()
       (nil)
     ,@body))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(let ((x 1))
  (while (< x 10)
    (princ x)
    (incf x)))

(defmacro till (test &body body)
  `(do ()
       (,test)
     @body))

;; a c like for
(defmacro cfor ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((>= ,var ,gstop))
       ,@body)))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((>= ,var ,gstop))
       ,@body)))

;; (cfor (i 0 10)
;;   (princ i))
