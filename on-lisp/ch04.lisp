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

(defun flattern (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;; (flattern '(a b c (d e)))

(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

;; (prune #'evenp '(1 2 (3 (4 5) 6) 7 8 (9)))


(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find2 fn (cdr lst))))))

;; (find2 #'evenp '(1 2 3))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

;; (before 'a 'b '(a b c))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

;; (after 'b 'a '(a b c))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
          :test test))

;; (duplicate 'a '(a a b b))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src)
             (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

(split-if #'(lambda (x)
              (> x 4))
          '(1 2 3 4 5 6 7 8 9))
