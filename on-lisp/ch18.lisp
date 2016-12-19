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

;; sparse condition
;; (with-array ((a 0 0) (d 1 1) (i 2 2))
;;     ar
;;   (values a d i))

(defmacro with-struct ((name . fields) struct &body body)
  (let ((gs (gensym)))
    `(let ((,gs ,struct))
       (let ,(mapcar #'(lambda (f)
                         `(,f (,(symb name f) ,gs)))
                     fields)
         ,@body))))

;; (defstruct visitor name title firm)
;; (setq theo (make-visitor :name "Theodebert"
;;                          :title 'king
;;                          :firm 'franks))
;; (with-struct (visitor- name firm title) theo
;;   (list name firm title))

(defmacro with-places (pat seq &body body)
  (let ((gseq (gensym)))
    `(let ((,gseq ,seq))
       ,(wplac-ex (destruc pat gseq #'atom) body))))

(defun wplac-ex (binds body)
  (if (null binds)
      `(progn ,@body)
      `(symbol-macrolet ,(mapcar #'(lambda (b)
                                     (if (consp (car b))
                                         (car b)
                                         b))
                                 binds)
         ,(wplac-ex (mapcan #'(lambda (b)
                                (if (consp (car b))
                                    (cdr b)))
                            binds)
                    body))))

;; (with-places (a b c) #(1 2 3)
;;   (list a b c))

;; (let ((lst '(1 (2 3) 4)))
;;   (with-places (a (b . c) d) lst
;;     (setf a 'uno)
;;     (setf c '(tre)))
;;   lst)


(defun match (x y &optional binds)
  (acond2
   ((or (eql x y) (eql x '_) (eql y '_)) (values binds t))
   ((binding x binds) (match it y binds))
   ((binding y binds) (match x it binds))
   ((varsym? x) (values (cons (cons x y) binds) t))
   ((varsym? y) (values (cons (cons y x) binds) t))
   ((and (consp x) (consp y) (match (car x) (car y) binds))
    (match (cdr x) (cdr y) it))
   (t (values nil nil))))


(defun varsym? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  (labels ((recbind (x binds)
             (aif (assoc x binds)
                  (or (recbind (cdr it) binds)
                      it))))
    (let ((b (recbind x binds)))
      (values (cdr b) b))))


;; (match '(p a b c a) '(p ?x ?y c ?x))

;; (match '(p ?x b ?y a) '(p ?y b c a))
;; (match '(a b c) '(a a a))

(defmacro if-match (pat seq then &optional else)
  `(aif2 (match ',pat ,seq)
         (let ,(mapcar #'(lambda (v)
                           `(,v (binding ',v it)))
                       (vars-in then #'atom))
           ,then)
         ,else))

(defun var? (x)
  (and (symbolp x) (eq (char (symbol-name x) 0) #\?)))
(defun vars-in (expr &optional (atom? #'atom))
  (if (funcall atom? expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr) atom?)
             (vars-in (cdr expr) atom?))))

(defun abab (seq)
  (if-match (?x ?y ?x ?y) seq
            (values ?x ?y)
            nil))

;; (abab '(hi ho hi ho))
