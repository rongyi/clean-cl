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
;; from on lisp
(defun group (source n)
  (if (zerop n)
      (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source
        (rec source nil)
        nil)))

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

;; test in *REAL* repl, not in slime, in slime, there is an error
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


;; (nthcdr 3 '(fail another string for))

(defun segment-reader (stream ch n)
  (if (> n 0)
      (let ((chars))
        (do ((curr (read-char stream)
                   (read-char stream)))
            ((char= ch curr))
          (push curr chars))
        (cons (coerce (nreverse chars) 'string)
              (segment-reader stream ch (- n 1))))))

#+cl-ppcre
(defmacro! match-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:scan
       ,(car ,g!args)
       ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args))))

#+cl-ppcre
(defun |#~-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((mode-char (read-char stream)))
    (cond
      ((char= mode-char #\m)
       (match-mode-ppcre-lambda-form
        (segment-reader stream
                        (read-char stream)
                        1)))
      ((char= mode-char #\s)
       (subst-mode-ppcre-lambda-form
        (segment-reader stream
                        (read-char stream)
                        2)))
      (t (error "Unkown #~~ mode character")))))

#+cl-ppcre
(set-dispatch-macro-character #\# #\~ #'|#~-reader|)

;; to evaluate this form
;; 1. first download cl-ppcre package
;; sbcl contain asdf
;; 2. (require "asdf")
;; 3. (load "/tmp/cl-ppcre-2.0.11/cl-ppcre.asd")
;; 4. (asdf:load-system :cl-ppcre)

;; (funcall #~s/abc/def/ "test abc testing abc")
;; (funcall #~m/abc/ "123abc")


(defmacro unit-of-timne (value unit)
  `(* ,value
      ,(case unit
         ((s) 1)
         ((m) 60)
         ((h) 3600)
         ((d) 86400)
         ((ms) 1/1000)
         ((us) 1/1000000))))
;; (testmacro (unit-of-timne 10 h))

;; (unit-of-timne 10 h)

;; nochaining version
(defmacro! defunits% (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of- quantity) (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x)) ,(cadr x)))
                      (group units 2))))))

;; (defunits% time s m 60 h 3600)
;; (unit-of-timne 1 h)


;; now we add chaning
(defun defunits-chaining% (u units)
  (let ((spec (find u units :key #'car)))
    (if (null spec)
        (error "Unkown unit ~a" u)
        (let ((chain (cadr spec)))
          (if (listp chain)
              (* (car chain)
                 (defunits-chaining%
                     (cadr chain)
                     units))
              chain)))))

;; (find 'h '((s 1) (m 60) (h (m 60))) :key #'car)
;; (defunits-chaining% 'h '((s 1) (m 60) (h (60 m))))

;; chaning version
(defmacro! defunits%% (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of- quantity) (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x))
                          ,(defunits-chaining%
                               (car x)
                               (cons `(,base-unit 1)
                                     (group units 2)))))
                      (group units 2))))))

;; incase circle dependancy
(defun defunits-chaining (u units prev)
  (if (member u prev)
      (error "~{ ~a~^ depends on ~}"
             (cons u prev)))
  (let ((spec (find u units :key #'car)))
    (if (null spec)
        (error "Unkown unit ~a" u)
        (let ((chain (cadr spec)))
          (if (listp chain)
              (* (car chain) (defunits-chaining (cadr chain) units (cons u prev)))
              chain)))))

;; (defunits-chaining 'h '((s 1) (m (1/60 h)) (h (60 m))) nil)

(defmacro! defunits (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of- quantity) (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un
            ((,base-unit) 1)
            ,@(mapcar (lambda (x)
                        `((,(car x))
                          ,(defunits-chaining
                               (car x)
                               (cons `(,base-unit 1)
                                     (group units 2))
                             nil)))
                      (group units 2))))))

;; (defunits time s m 60 h (60 m))
;; (unit-of-time 1 h)

(defun tree-leaves% (tree result)
  (if tree
      (if (listp tree)
          (cons (tree-leaves% (car tree)
                              result)
                (tree-leaves% (cdr tree)
                              result))
          result)))

;; (tree-leaves%
;;  '(2 (nil t (a . b)))
;;  'leaf)

;; (sort '(5 1 2 4 3 8 9 6 7) #'<)

(defun predicate-splitter (orderp splitp)
  (lambda (a b)
    (let ((s (funcall splitp a)))
      (if (eq s (funcall splitp b))
          (funcall orderp a b)
          s))))

;; (sort '(5 1 2 4 3 8 9 6 7)
;;       (predicate-splitter #'< #'evenp))

(defun tree-leaves%% (tree test result)
  (if tree
      (if (listp tree)
          (cons (tree-leaves%% (car tree) test result)
                (tree-leaves%% (cdr tree) test result))
          (if (funcall test tree)
              (funcall result tree)
              tree))))

(tree-leaves%% '(1 2 (3 4 (5 6)))
               (lambda (x)
                 (and (numberp x) (evenp x)))
               (lambda (x)              ;the x here is not used
                 'even-number))

;; so wen can tell compiler explicitly
;; (tree-leaves%% '(1 2 (3 4 (5 6)))
;;                (lambda (x)
;;                  (declare (ignorable x))
;;                  (and (numberp x) (evenp x)))
;;                (lambda (x)              ;the x here is not used
;;                  (declare (ignorable x))
;;                  'even-number))

;; simple wrapper
(defmacro tree-leaves (tree test result)
  `(tree-leaves%%
    ,tree
    (lambda (x)
      (declare (ignorable x))
      ,test)
    (lambda (x)
      (declare (ignorable x))
      ,result)))

;; simple wrapper
;; (tree-leaves
;;  '(1 2 (3 4 (5 . 6)))
;;  (and (numberp x) (evenp x))
;;  'even-number)

(defmacro cxr% (x tree)
  (if (null x)
      tree
      `(,(cond
           ((eq 'a (cadr x)) 'car)
           ((eq 'd (cadr x)) 'cdr)
           (t (error "Non A/D symbol")))
         ,(if (= 1 (car x))
              `(cxr% ,(cddr x) ,tree)
              `(cxr% ,(cons (- (car x) 1) (cdr x))
                     ,tree)))))

;; (macroexpand '(cxr% (1 a 2 d) '(1 2 3 4 5)))
;; (cxr% (2 a 10 d) '(1 2 3 4 5))

(format nil "~r" 109887)


(defun cxr-symbol-p (s)
  (if (symbolp s)
      (let ((chars (coerce (symbol-name s) 'list)))
        (and
         (< 6 (length chars))
         (char= #\C (car chars))
         (char= #\R (car (last chars)))
         (null (remove-if
                #'(lambda (c)
                    (or (char= c #\A)
                        (char= c #\D)))
                (cdr (butlast chars))))))))

;; (cxr-symbol-p 'cadadadaadadr)

;; (length (coerce (symbol-name 'cadadadaadadr) 'list))

(defun cxr-symbol-to-cxr-list (s)
  (labels ((collect (l)
             (if l
                 (list* 1
                        (if (char= (car l) #\A)
                            'A
                            'D)
                        (collect (cdr l))))))
    (collect (cdr (butlast (coerce (symbol-name s) 'list))))))

(defmacro with-all-cxrs (&rest forms)
  `(labels (,@(mapcar (lambda (s)
                        `(,s (l)
                             (cxr% ,(cxr-symbol-to-cxr-list s)
                                   l)))
                      (remove-duplicates (remove-if-not #'cxr-symbol-p (flatten forms)))))
     ,@forms))

;; (with-all-cxrs (cons (cadadadr '(1 2 3))
;;                      (caaaaaaar '(4 5 6))))

(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
          (lambda (d)
            `(,(if (eq t (car d))
                   t
                   (list (car d)))
               (apply (lambda ,@(cdr d))
                      ,(if (eq t (car d))
                           g!args
                           `(cdr ,g!args)))))
          ds))))

(setf (symbol-function 'count-test)
      (let ((count 0))
        (dlambda
         (:inc (n) (incf count n))
         (:dec (n) (decf count n))
         (:reset () (setf count 0))
         (:bound (lo hi)
                 (setf count
                       (min hi
                            (max lo
                                 count)))))))

;; (count-test :inc)
;; (count-test :dec)
;; (count-test :bound -10 100)
;; (count-test :inc)
;; (count-test :inc 1000)

(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
               collect (symb 'a i))
     ,(funcall (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character #\# #\` #'|#`-reader|)

;; (mapcar (lambda (a)
;;           (list a ''empty))
;;         '(a b c))

;; (mapcar (lambda (a)
;;           `(,a 'empty))
;;         '(a b c))
