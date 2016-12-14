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
