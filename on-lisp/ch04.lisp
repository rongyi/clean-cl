(proclaim '(inline last1 single append1 conc1 mklist))
;; when utilities are small, they form a layer of abstraction so thin that it starts to be transparent

(defun last1 (lst)
  (car (last lst)))

(defun single (lst)
  (and (consp lst)
       (not (cdr lst))))

(defun append1 (lst obj)
  (apend lst (list obj)))


(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj)
      obj
      (list obj)))

(defun longer (x y)
  (labels ((cmp (x y)
             (and (consp x)
                  (or (null y)
                      (cmp (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (cmp x y)
        (> (length x)
           (length y)))))

;; (longer '(a b c e e e e e e e e e e e) '(e e e e e))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val
            (push x acc))))             ;push original value or the funcalled value?
    (nreverse acc)))

;; (filter #'evenp '(1 2 3 4))

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

;; (nthcdr 0 '(a b c))
;; (nthcdr 1 '(a b c))
;; (nthcdr 2 '(a b c))
;; (subseq '(a b c) 0 2)
;; (group '(a b c d e) 2)
