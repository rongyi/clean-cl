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

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))

;; (mapcar #'list '(a b c) '(d e f))

;; (defmacro! square (o!x)
;;   `(* ,g!x ,g!x))

;; (testmacro (square (incf x)))

;; '(football-game
;;   (game-started-at
;;    #.(get-internal-real-time))
;;   (coin-flip
;;    #.(if (zerop (random 2)) 'heads 'tails)))

;; `(football-game
;;   (game-started-at
;;    ,(get-internal-real-time))
;;   (coin-flip
;;    ,(if (zerop (random 2)) 'heads 'tails)))

;; chapter 4
;; '`q

;; (let ((s '(b c d)))
;;   `(a . ,s))

;; (let ((s '(b c d)))
;;   `(a ,@s e))

;; (defvar to-splice '(b c d))
;; evaluate twice
;; `(a ,.to-splice e)

(let ((let '`(let ((let ',let))
               ,let)))
  `(let ((let ',let)) ,let))

(defun |#"-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((chars nil))
    (do ((prev (read-char stream) curr)
         (curr (read-char stream) (read-char stream)))
        ((and (char= prev #\") (char= curr #\#)))
      (push prev chars))
    (coerce (nreverse chars) 'string)))

(set-dispatch-macro-character #\# #\" #'|#"-reader|)

(defun |#>-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((chars))
    (do ((curr (read-char stream)
               (read-char stream)))
        ((char= #\newline curr))
      (push curr chars))
    (let* ((pattern (nreverse chars))
           (pointer pattern)
           (output))
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((null pointer))
        (push curr output)
        (setf pointer
              (if (char= (car pointer) curr)
                  (cdr pointer)
                  pattern))
        (if (null pointer)
            (return)))
      (coerce
       (nreverse
        (nthcdr (length pattern) output))
       'string))))

(set-dispatch-macro-character
 #\# #\> #'|#>-reader|)
