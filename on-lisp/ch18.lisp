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
