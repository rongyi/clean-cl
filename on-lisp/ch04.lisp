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
