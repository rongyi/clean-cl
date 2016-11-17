
(defun ry/complement (fn)
  #'(lambda (&rest args)
      (not (apply fn args))))

;; (remove-if (ry/complement #'oddp)
;;            '(1 2 3 4 5 6))

;; shchme convention of appending ! to the names of functions with side-effects.
