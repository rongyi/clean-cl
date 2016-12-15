;; macros returning functions
;; (funcall (compose #'list #'1+) 2)

(defmacro fn (expr)
  `#',(rbuild expr))

(defun rbuild (expr)
  (if (or (atom expr) (eq (car expr) 'lambda))
      expr
      (if (eq (car expr) 'compose)
          (build-compose (cdr expr))
          (build-call (car expr) (cdr expr)))))

(defun build-call (op fns)
  (let ((g (gensym)))
    `(lambda (,g)
       (,op ,@(mapcar #'(lambda (f)
                          `(,(rbuild f) ,g))
                      fns)))))

(defun build-compose (fns)
  (let ((g (gensym)))
    `(lambda (,g)
       ,(labels ((rec (fns)
                   (if fns
                       `(,(rbuild (car fns))
                          ,(rec (cdr fns)))
                       g)))
          (rec fns)))))

;; (funcall (fn (and integerp oddp)) 7)
;; (testmacro (fn (and integerp oddp)))

;; (funcall  (fn (compose list 1+ truncate)) 3.7)

;; (testmacro (fn (compose list 1+ truncate)))

;; (mapcar (fn (and integerp oddp))
;;         '(c 3 p 0))
;; (mapcar (fn (or integerp symbolp))
;;         '(c 3 p 0.2))

;; (map1-n (fn (if oddp 1+ identity))
;;         6)

;; (testmacro (fn (list 1- identity 1+)))
;; (mapcar (fn (list 1- identity 1+))
;;         '(1 2 3))

;; (remove-if (fn (or (and integerp oddp)
;;                    (and consp cdr)))
;;            '(1 (a b) c (d) 2 3.4 (e f g)))

;; (defmacro alrec (rec &optional base)
;;   "cltl1 version"
;;   (let ((gfn (gensym)))
;;     `(lrec #'(lambda (it ,gfn)
;;                (labels ((rec () (funcall ,gfn)))
;;                  ,rec))
;;            ,base)))

(defmacro alrec (rec &optional base)
  "cltl2 version"
  (let ((gfn (gensym)))
    `(lrec #'(lambda (it ,gfn)
               (symbol-macrolet ((rec (funcall ,gfn)))
                 ,rec))
           ,base)))

;; (funcall (alrec (and (oddp it) rec) t) '(1 3 5))
;; (testmacro (alrec (and (oddp it) rec) t))

(defmacro on-cdrs (rec base &rest lsts)
  `(funcall (alrec ,rec #'(lambda () ,base)) ,@lsts))

(defun our-length (lst)
  (on-cdrs (1+ rec) 0 lst))

;; (our-length '(1 2 3))

(defun our-every (fn lst)
  (on-cdrs (and (funcall fn it) rec) t lst))

;; (our-every #'oddp '(1 3 3))

(defun our-copy-list (lst)
  (on-cdrs (cons it rec) nil lst))
;; (our-copy-list '(a 1 3))

(defun our-remove-duplicates (lst)
  (on-cdrs (adjoin it rec) nil lst))
;; (our-remove-duplicates '(a a a b))
(defun our-find-if (fn lst)
  (on-cdrs (if (funcall fn it) it rec) nil lst))

;; (our-find-if #'oddp '(1 2 3))
(defun our-some (fn lst)
  (on-cdrs (or (funcall fn it) rec) nil lst))

;; (our-some #'oddp '(1 2 3))
;; (union '(a b) (union '(b c) '(c d)))
(defun unions (&rest sets)
  (on-cdrs (union it rec) (car sets) (cdr sets)))

;; (testmacro (unions '(a b) '(c d)))

;; (unions '(a b) '(c d) '(e d))
(defun intersections (&rest sets)
  (unless (some #'null sets)
    (on-cdrs (intersection it rec)
             (car sets)
             (cdr sets))))

(defun differences (set &rest outs)
  (on-cdrs (set-difference rec it) set outs))

(defun maxmin (args)
  (when args
    (on-cdrs (multiple-value-bind (mx mn) rec
               (values (max mx it) (min mn it)))
             (values (car args) (car args))
             (cdr args))))

;; (maxmin '(1 2 3))
(defmacro atrec (rec &optional (base 'it))
  "cltl2 version"
  (let ((lfn (gensym))
        (rfn (gensym)))
    `(trec #'(lambda (it ,lfn ,rfn)
               (symbol-macrolet ((left (funcall ,lfn))
                                 (right (funcall ,rfn)))
                 ,rec))
           #'(lambda (it) ,base))))

(defmacro on-trees (rec base &rest trees)
  `(funcall (atrec ,rec ,base) ,@trees))

;; (funcall (atrec (cons left right)) '((a b c)))
;; (testmacro (atrec (cons left right)))
;; (funcall (atrec (or left right) (and (oddp it) it) ) '((1 2 3) 1))

(defun our-copy-list (tree)
  (on-trees (cons left right) it tree))
(defun count-leaves (tree)
  (on-trees (+ left (or right 1)) 1 tree))

(defun flatten (tree)
  (on-trees (nconc left right) (mklist it) tree))

(defun rfind-if (fn tree)
  (on-trees (or left right)
            (and (funcall fn it) it)
            tree))

;; lazy evaluation
(defconstant unforced (gensym))
(defstruct delay forced closure)

(defmacro delay (expr)
  (let ((self (gensym)))
    `(let ((,self (make-delay :forced unforced)))
       (setf (delay-closure ,self)
             #'(lambda ()
                 (setf (delay-forced ,self) ,expr)))
       ,self)))

(defun force (x)
  (if (delay-p x)
      (if (eq (delay-forced x) unforced)
          (funcall (delay-closure x))
          (delay-forced x))
      x))

;; (let ((x 2))
;;   (setq d (delay (1+ x)))
;;   (force d))
