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
