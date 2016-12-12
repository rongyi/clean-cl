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
