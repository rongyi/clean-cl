;; macro define macro
;; Fortunately, in a language like Lisp you don't have to live with all the decisions of the designers.


(defmacro dbind (&rest args)
  `(destructuring-bind ,@args))

;; (defmacro mvbind (&rest args)
;;   `(multiple-value-bind ,@args))

;; Nested backquotes are notoriously hard to understand
;; 嵌套backquote太难懂了

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

;; (testmacro (abbrev mvbind multiple-value-bind))

(defmacro abbrevs (&rest names)
  `(progn
     ,@(mapcar #'(lambda (pair)
                   `(abbrev ,@pair))
               (group names 2))))

;; (abbrevs mvbind multiple-value-bind
;;          mvsetq multiple-value-setq)

(defmacro propmacro (propname)
  `(defmacro ,propname (obj)
     `(get ,obj ',',propname)))

(defmacro propmacros (&rest props)
  `(progn
     ,@(mapcar #'(lambda (p)
                   `(propmacro ,p))
               props)))

(defmacro a+ (&rest args)
  (a+expand args nil))

(defun a+expand (args syms)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(a+expand (cdr args)
                      (append syms (list sym)))))
      `(+ ,@syms)))

;; (testmacro (a+ 7 (* it 100)))
;; (a+ 7 (* it 100))

;; (defun mass-cost (menu-price)
;;   (a+ menu-price (* it .05) (* it 3)))
;; (mass-cost 7.95)

(defmacro alist (&rest args)
  (alist-expand args nil))

(defun alist-expand (args syms)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(alist-expand (cdr args)
                          (append syms (list sym)))))
      `(list ,@syms)))

;; (alist 1 (+ 2 it) (+ 2 it))

(defmacro defanaph (name &optional calls)
  (let ((calls (or calls (pop-symbol name))))
    `(defmacro ,name (&rest args)
       (anaphex args (list ',calls)))))

(defun anaphex (args expr)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(anaphex (cdr args)
                     (append expr (list sym)))))
      expr))

(defun pop-symbol (sym)
  (intern (subseq (symbol-name sym) 1)))

;; (pop-symbol 'hell)

;; (defanaph a+)
;; (defanaph alist)

(defmacro defanaph (name &optional &key calls (rule :all))
  (let* ((opname (or calls (pop-symbol name)))
         (body (case rule
                 (:all `(anaphex1 args '(,opname)))
                 (:first `(anaphex2 ',opname args))
                 (:place `(anaphex3 ',opname args)))))
    `(defmacro ,name (&rest args)
       ,body)))

(defun anaphex1 (args call)
  (if args
      (let ((sym (gensym)))
        `(let* ((,sym ,(car args))
                (it ,sym))
           ,(anaphex1 (cdr args)
                      (append call (list sym)))))
      call))

(defun anaphex2 (op args)
  `(let ((it ,(car args)))
     (,op it ,@(cdr args))))

(defun anaphex3 (op args)
  `(_f (lambda (it)
         (,op it ,@(cdr args)))
       ,(car args)))
