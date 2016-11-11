(setf part (list 'b 'c))
(setf whole (cons 'a part))
(tailp part whole)
(defun our-tail-p (x y)
  (or (eq x y)
      (and (consp y)
           (our-tail x (cdr y)))))
(our-tail-p part whole)
(our-tail-p 'a 'a)

(defun ry/copy-list (lst)
  (if (null lst)
      nil
      (cons (car lst) (ry/copy-list (cdr lst)))))

(defun ry/copy-tree (tr)
  (if (atom tr)
      tr
      (cons (ry/copy-tree (car tr))
            (ry/copy-tree (cdr tr)))))
