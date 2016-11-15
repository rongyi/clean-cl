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

(defun make-queue ()
  (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

(setf q1 (make-queue))
(enqueue 'a q1)
(enqueue 'b q1)
(enqueue 'c q1)
(dequeue q1)
