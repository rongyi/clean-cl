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

;; (nif -1 'pos 'zero 'neg)

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

;; one level less quote
;; (mapcar (lambda (a)
;;           `(,a 'empty))
;;         '(a b c))

;; using the new read macro, no lambda needed
;; (mapcar #`(,a1 'empty)
;;         '(a b c))

(let ((vars '(a b c)))
  (mapcar #2`(,a1 ',a2)
          vars
          (loop for v in vars
             collect (gensym (symbol-name v)))))

(defmacro alet% (letargs &rest body)
  `(let ((this)
         ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     this))

(setf  (symbol-function 'alet%-test)
       (alet% ((sum) (mul) (expt))
              (funcall this :reset)
              (dlambda
               (:reset ()
                       (psetq sum 0
                              nul 1
                              expt 2))
               (t (n)
                  (psetq sum (+ sum n)
                         mul (* nul n)
                         expt (expt expt n))
                  (list sum mul expt)))))
;; (alet%-test 2)
;; (alet%-test :reset)
;; (alet%-test 0.5)
(alet% ((sum) (mul) (expt))
       (funcall this :reset)
       (dlambda
        (:reset ()
                (psetq sum 0
                       nul 1
                       expt 2))
        (t (n)
           (psetq sum (+ sum n)
                  mul (* nul n)
                  expt (expt expt n))
           (list sum mul expt))))

;; with indirection
(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))

(defmacro alambda (params &body body)
  `(labels ((self ,params ,@body))
     #'self))

(setf (symbol-function 'alet-test)  (alet ((acc 0))
                                          (alambda (n)
                                            (if (eq n 'invert)
                                                (setq this
                                                      (lambda (n)
                                                        (if (eq n 'invert)
                                                            (setq this #'self)
                                                            (decf acc n))))
                                                (incf acc n)))))


;; (alet-test 10)
;; (alet-test 'invert)

;; 此书主要讨论的Lisp的macro技巧，而作者在开篇就毫无保留地对Paul Graham的《On Lisp》充满了溢美之词，后面就开始就直接使用了On Lisp中的很多基础库(比如symb,flatten,group)，所以这本书的阅读顺序推荐放在On Lisp之后。

;; 作者从Lisp最简单的let over lambda讲起，到后面的alet over alambdadlambda等等逐渐展开，渐渐的从小模块开始堆起，然后发现凑在一起的效果真是脑洞大开的那种感觉，原来代码还可以这么写。write program to program。国外的创业公司也有开始在后端部署Lisp[fn:1] Alan Perlis讲过： A language that doesn't affect the way you think about programming, is not worth knowing.[fn:2] 从这个角度来看Lisp绝对是值得学习的一门语言，像Inception那部电影： A dream in a dream讲述的那样，Lisp作为一门编程语言具有极其强悍的进化特性，引用此书的一句话： In other language you can walk north, south, east, and west, but lisp also gives you the option of going up。 Lisp开了一个新的维度，而打开这个维度的就是macro。

;; 书评的名称来自韩国电影新世界，这是从我跟人角度学习Lisp的最大体会。

;; * Footnotes
;; [fn:1] http://tech.grammarly.com/blog/posts/Running-Lisp-in-Production.html
;; [fn:2] http://pu.inf.uni-tuebingen.de/users/klaeren/epigrams.html

(defmacro alet-fsm (&rest states)
  `(macrolet ((state (s)
                `(setq this #',s)))
     (labels (,@states) #',(caar states))))


(alet ((acc 0))
      (alet-fsm (going-up (n)
                          (if (eq n 'invert)
                              (state going-down)
                              (incf acc n)))
                (going-down (n)
                            (if (eq n 'invert)
                                (state going-up)
                                (decf acc n)))))


(defmacro! ichain-before (&rest body)
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             ,@body
             (apply ,g!indir-env
                    ,g!temp-args)))))


(setf (symbol-function 'test-ichain)
      (alet ((acc 0))
            (ichain-before
             (format t "Changing from ~a~%" acc))
            (ichain-before
             (format t "B~%"))
            (ichain-before
             (format t "C~%"))
            (ichain-before
             (format t "D~%"))
            (lambda (n)
              (incf acc n))))


(alet ((acc 0))
      (ichain-before
       (format t "Changing from ~a~%" acc))
      (lambda (n)
        (incf acc n)))
;; note the repl output
;; (test-ichain 2)

(defmacro! ichain-after (&rest body)
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             (prog1
                 (apply ,g!indir-env
                        ,g!temp-args)
               ,@body)))))

(setf (symbol-function 'ichain-after-test)
      (alet ((acc 0))
            (ichain-before
             (format t "Changing from ~a~%" acc))
            (ichain-after
             (format t "Changed to ~a~%" acc))
            (lambda (n)
              (incf acc n))))

;; (ichain-after-test 2)

(defmacro! ichain-intercept% (&rest body)
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             (block intercept
               (prog1
                   (apply ,g!indir-env
                          ,g!temp-args)
                 ,@body))))))

(setf (symbol-function 'ichain-intercept-test)
      (alet ((acc 0))
            (ichain-intercept%
             (when (< acc 0)
               (format t "Acc went negative~%")
               (setq acc 0)
               (return-from intercept acc)))
            (lambda (n)
              (incf acc n))))

;; (ichain-intercept-test 2)

(defmacro! ichain-intercept (&rest body)
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             (block ,g!intercept
               (macrolet ((intercept (v)
                            `(return-from
                              ,',g!intercept
                               ,v)))
                 (prog1
                     (apply ,g!indir-env
                            ,g!temp-args)
                   ,@body)))))))

(setf (symbol-function 'ichain-intercept-test)
      (alet ((acc 0))
            (ichain-intercept
             (when (< acc 0)
               (format t "Acc went negative~%")
               (setq acc 0)
               (intercept acc)))
            (lambda (n)
              (incf acc n))))

;; (ichain-intercept-test -90)
(defmacro alet-hotpatch% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest args)
       (if (eq (car args) ':hotpatch)
           (setq this (cadr args))
           (apply this args)))))

(setf (symbol-function 'hotpatch-test)
      (alet-hotpatch% ((acc 0))
                      (lambda (n)
                        (incf acc n))))
;; (hotpatch-test 1)
;; (hotpatch-test :hotpatch
;;                (let ((acc 0))
;;                  (lambda (n)
;;                    (incf acc (* 2 n)))))
;; (hotpatch-test 2)


(defmacro alet-hotpatch (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (dlambda
      (:hotpatch (closure)
                 (setq this closure))
      (t (&rest args)
         (apply this args)))))

;; (setf (symbol-function 'hotpatch-test)
;;       (alet-hotpatch ((acc 0))
;;                      (lambda (n)
;;                        (incf acc n))))
;; (hotpatch-test 1)
;; (hotpatch-test :hotpatch
;;                (let ((acc 0))
;;                  (lambda (n)
;;                    (incf acc (* 2 n)))))
;; (hotpatch-test 2)

;; anaphor closing

(defmacro! let-hotpatch (letargs &rest body)
  `(let ((,g!this) ,@letargs)
     (setq ,g!this ,@(last body))
     ,@(butlast body)
     (dlambda
      (:hotpatch (closure)
                 (setq ,g!this closure))
      (t (&rest args)
         (apply ,g!this args)))))


(defun let-binding-transform (bs)
  (if bs
      (cons
       (cond ((symbolp (car bs))
              (list (car bs)))
             ((consp (car bs))
              (car bs))
             (t (error "Bad let bindings")))
       (let-binding-transform (cdr bs)))))

(defmacro sublet (bindings% &rest body)
  (let ((bindings (let-binding-transform bindings%)))
    (setq bindings
          (mapcar (lambda (x)
                    (cons (gensym (symbol-name (car x))) x))
                  bindings))
    `(let (,@(mapcar #'list
                     (mapcar #'car bindings)
                     (mapcar #'caddr bindings)))
       ,@(tree-leaves
          body
          #1=(member x bindings :key #'cadr)
          (caar #1#)))))

;; (sublet ((a 0))
;;         (list a))

(defmacro sublet* (bindings &rest body)
  `(sublet ,bindings
           ,@(mapcar #'macroexpand-1 body)))

(defun pandoriclet-get (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1)) ,(car a1))
               letargs)
     (t (error "Unkown pandoric get: ~a"
               sym))))

(defun pandoriclet-set (letargs)
  `(case sym
     ,@(mapcar #`((,(car a1))
                  (setq ,(car a1) val))
               letargs)
     (t (error "Unkown pandoric set: ~a" sym val))))

(defmacro pandoriclet (letargs &rest body)
  (let ((letargs (cons '(this) (let-binding-transform letargs))))
    `(let (,@letargs)
       (setq this ,@(last body))
       ,@(butlast body)
       (dlambda
        (:pandoric-get (sym)
                       ,(pandoriclet-get letargs))
        (:pandoric-set (sym val)
                       ,(pandoriclet-set letargs))
        (t (&rest args)
           (apply this args))))))

;; (setf (symbol-function 'pandoric-test)
;;       (pandoriclet ((acc 0))
;;                    (lambda (n)
;;                      (incf acc n))))

;; (pandoric-test 3)

;; (pandoric-test :pandoric-get 'acc)
;; (pandoric-test :pandoric-set 'acc 1000)
;; now test again
;; (pandoric-test 3)
;; (pandoric-test :pandoric-get 'this)

(declaim (inline get-pandoric))
(defun get-pandoric (box sym)
  (funcall box :pandoric-get sym))
(defsetf get-pandoric (box sym) (val)
  `(progn
     (funcall ,box :pandoric-set ,sym ,val)
     ,val))

;; (get-pandoric #'pandoric-test 'acc)
;; (setf (get-pandoric #'pandoric-test 'acc) -1000)
;; (pandoric-test 3)
