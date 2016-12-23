;; 这个函数只是在一个stream中找到target，找到一次之后就结束了，
;; 后面出现不出现已经不重要了，因为这个函数始终会返回T， shit
(defun block-scanner (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
         (curr trig))
    (lambda (data-string)
      (let ((data (coerce data-string 'list))
            (ret nil))
        (dolist (c data)
          (if curr
              (setq curr
                    (if (char= (car curr) c)
                        (cdr curr)
                        trig))))
        (not curr)))))

;; (defvar scanner (block-scanner "jihad"))
;; (funcall scanner "jiha")
;; (funcall scanner "d")
;; (funcall scanner "jiha")
;; (funcall scanner "jihad")
;; (funcall scanner "jihad")
;; (funcall scanner "we will ji")
;; (funcall scanner "had")
;; (funcall scanner "jihad")
;; (funcall scanner "jihajihad")


(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x)
                           (rec (cdr x) acc))))))
    (rec x nil)))

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))

;; (g!-symbol-p 'HELLO)
;; (g!-symbol-p 'g!hello)

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar #'(lambda (s)
                         `(,s (gensym ,(subseq (symbol-name s)
                                               2))))
                     syms)
         ,@body))))

(defmacro/g! nif (expr pos zero neg)
  `(let ((,g!result ,expr))
     (cond ((plusp ,g!result) ,pos)
           ((zerop ,g!result) ,zero)
           (t ,neg))))

;; (let ((*print-circle* t))
;;   (print
;;    (macroexpand '(nif -1 'pos 'zero 'neg))))
;; from now on all the source code will be put to one file!!!
(defmacro testmacro (expr)
  `(pprint (macroexpand-1 ',expr)))


(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args)
      (princ a s))))

;; (mkstr pi "sdfasdf" 'pi)
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

;; (symb 'make 'str)

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))

;; (o!-symbol-to-g!-symbol 'O!MyGod)
;; (symbol-name 'name)
