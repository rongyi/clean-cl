(defmacro nil! (var)
  (list 'setq var nil))

(let ((n 10))
  (nil! n))
(macroexpand-1 '(nil! n))

;; Here's the stroke of genius: the output of the parse consists of
;; lists of Lisp Objects. With macros, we can manipulate the program
;; while it's in this intermediate from between parse and compiler.

;; backquote section
(setq a 1 b 2 c 3)
`(a ,b c)

;; one comma counteracts the effect of one backquote
;; so commas must match backquote.
;; The general rule is: a comma surrounded by n commas
;; must be surrounded by at least n + 1 backquotes.

(defmacro nil! (var)
  `(setq ,var nil))

;; withbackquote
(defmacro nif (expr pos zero neg)
  `(case (truncate (signum ,expr))
     (1 ,pos)
     (0 ,zero)
     (-1 ,neg)))

(mapcar #'(lambda (x)
            (nif x 'p 'z 'n))
        '(0 2.5 -6))

;; without backquote
(defmacro nil (expr pos zero neg)
  (list 'case
        (list 'truncate (list 'signum expt))
        (list 1 pos)
        (list 0 zero)
        (list -1 neg)))

(mapcar #'(lambda (x)
            (nif x 'p 'z 'n))
        '(0 2.5 -6))

;; comma at
(setq b '(1 2 3))
;; `(a ,b c)
;; `(a ,@b c)
;; comma @ restrictions
;; 1. comma at must occur within a sequence
;; 2. the object to be spliced must be a list, unless it occures last
(defmacro ry/when (test &body body)
  `(if ,test
       (progn
         ,@body)))

;; backquote is so often used in macro definitions that people sometimes think of backquote as poart
;; of defmacro

;; In programming, the best way to learn is often to begin experimenting as
;; soon as possible.

(defmacro memq (obj lst)
  `(member ,obj ,lst :test #'eq))
;; (macroexpand-1 '(memq 1 '(1 2 3)))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

;; how to test macro
(defmacro testmacro (expr)
  `(pprint (macroexpand-1 ',expr)))

;; (testmacro '(while (able) (laugh)))
;; macroexpand-1 can also be used to expand the Common Lisp standard macros

;; Destructuring describes the situation where this sort of positional assignment is done for arbitrary
;; list structures, as well as flat lists like (x y z)

(defmacro ry/dolist ((var list &optional result) &body body)
  `(progn
     (mapc #'(lambda (,var) ,@body)
           ,list)
     (let ((,var nil))
       ,result)))
(testmacro (ry/dolist (x '(a b c))))

;; mapc is like mapcar except that the results of applying function are not accumulated. The list argument is returned.
;; (mapcar #'(lambda (x)
;;             (1+ (* x 2)))
;;         '(1 2 3 4))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
     (when ,var
       ,@body)))

(defmacro ry/expander (name)
  `(get ,name 'expander))

(defmacro ry/defmacro (name params &body body)
  (let ((g (gensym)))
    `(progn
       (setf (ry/expander ',name)
             #'(lambda (,g)
                 (block ,name
                   (destructuring-bind ,params (cdr ,g)
                     ,@body))))
       ',name)))

(defun ry/macroexpand-1 (expr)
  (if (and (consp expr)
           (ry/expander (car expr)))
      (funcall (ry/expander (car expr)) expr)
      expr))
