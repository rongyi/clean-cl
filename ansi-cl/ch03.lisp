(setf x (cons 'a nil))
(car x)
(cdr x)
(setf x (cons 'a x))
(car (cdr x))
(setf y (list 'a 'b 'c '\'))

(defun ry-listp (lst)
  (or (null lst) (consp lst)))
(defun ry-atomp (x)
  (not (consp x)))
(eql (cons 'a nil) (cons 'a nil))
(setf x (cons 'a nil))
(eql x x)
;; print out is the same
(equal x (cons 'a 'nil))

(defun our-equal (x y)
  (or (eql x y)
      (and (consp x)
           (consp y)
           (our-equal (car x) (car y))
           (our-equal (cdr x) (cdr y)))))

(our-equal x (cons 'a nil))

(setf x '(a b c))
(setf y x)
(eql x y)
(equal x y)

(setf x '(a b c)
      y (copy-list x))
(equal x y)
(eql x y)

(defun our-copy-list (lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list (cdr lst)))))

(append '(a b ) '(c d) '(e))

;; in chinese:
(defun n-elts (count number)
  (if (> count 1)
      (list count number)
      number))

(defun compress (x)
  (if (consp x)
      (do-compress (car x) 1 (cdr x))
      x))

(defun do-compress (elt n lst)
  (if (null lst)
      (list (n-elts n elt))
      (let ((next (car lst)))
        (if (eql next elt)
            (do-compress elt (+ n 1) (cdr lst))
            (cons (n-elts n elt)
                  (do-compress next 1 (cdr lst)))))))

(defun list-of (count elt)
  (if (zerop count)
      nil
      (cons elt (list-of (1- count) elt))))
(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
            (rest (uncompress (cdr lst))))
        (if (consp elt)
            (append (apply 'list-of elt)
                    rest)
            (cons elt rest)))))

(nth 0 '(a b c))
(nthcdr 2 '(a b c))

(defun our-nthcdr (n lst)
  (if (= n 0)
      lst
      (our-nthcdr (1- n) (cdr lst))))

(mapcar #'(lambda (x)
          (+ 100 x))
        '(1 2 3 4 5))

;; the last ww is omitted
(mapcar #'list
        '(a b c)
        '(x y z ww))

(maplist #'(lambda (x)
             x)
         '(a b c d))

(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
            (our-copy-tree (cdr tr)))))

(subst 'y 'x '(and (intergerp x) (zerop (mod x 2))))

(defun our-subst (new old tree)
  (if (eql tree old)
      new
      (if (atom tree)
          tree
          (cons (our-subst new old (car tree))
                (our-subst new old (cdr tree))))))


(member 'b '(a b c))
(member 'b '(a b c) :test #'equal)
(member '(a) '((a) (z)) :test #'equal)

(member 'a '((a b) (b c)) :key #'car)

(member 2 '((a) (2)) :test #'equal :key #'car)

(member-if #'oddp '(2 3 5))

(defun our-member-if (fn lst)
  (when (consp lst)
       (if (funcall fn (car lst))
           lst
           (our-member-if fn (cdr lst)))))

(adjoin 'b '(a b c))
(adjoin 'z '(a b c))
