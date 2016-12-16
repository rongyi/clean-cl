;; Establishing bindings en masse is an attractive idea.


(defun destruc (pat seq &optional (atom? #'atom) (n 0))
  (if (null pat)
      nil
      (let ((rest (cond ((funcall atom? pat) pat)
                        ((eq (car pat) '&rest) (cadr pat))
                        ((eq (car pat) '&body) (cadr pat))
                        (t nil))))
        (if rest
            `((,rest (subseq ,seq ,n)))
            (let ((p (car pat))
                  (rec (destruc (cdr pat) seq atom? (1+ n))))
              (if (funcall atom? p)
                  (cons `(,p (elt ,seq ,n))
                        rec)
                  (let ((var (gensym)))
                    (cons (cons `(,var (elt ,seq ,n))
                                (destruc p var atom?))
                          rec))))))))

;; (destruc '(a b c) 'seq #'atom)
;; (destruc '(a (b . c) &rest d) 'seq)

(defun dbind-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(let ,(mapcar #'(lambda (b)
                         (if (consp (car b))
                             (car b)
                             b))
                     binds)
         ,(dbind-ex (mapcan #'(lambda (b)
                                (if (consp (car b))
                                    (cdr b)))
                            binds)
                    body))))

;; (destruc '(a (b . c) &rest d) 'seq)
;; (dbind-ex (destruc '(a (b . c) &rest d) 'seq) '(body))

(defmacro with-matrix (pats ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(let ((row -1))
               (mapcan #'(lambda (pat)
                           (incf row)
                           (setq col -1)
                           (mapcar #'(lambda (p)
                                       `(,p (aref ,gar
                                                  ,row
                                                  ,(incf col))))
                                   pat))
                       pats))
         ,@body))))

(defmacro with-array (pat ar &body body)
  (let ((gar (gensym)))
    `(let ((,gar ,ar))
       (let ,(mapcar #'(lambda (p)
                         `(,(car p) (aref ,gar ,@(cdr p))))
                     pat)
         ,@body))))

;; (setq ar (make-array '(3 3)))
;; (ry/for (r 0 2)
;;      (ry/for (c 0 2)
;;           (setf (aref ar r c) (+ (* r 10) c))))

;; (with-matrix ((a b c)
;;               (d e f)
;;               (g h i)) ar
;;   (list a b c d e f g h i))
