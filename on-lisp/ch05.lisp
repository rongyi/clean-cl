
(defun ry/complement (fn)
  #'(lambda (&rest args)
      (not (apply fn args))))

;; (remove-if (ry/complement #'oddp)
;;            '(1 2 3 4 5 6))

;; shchme convention of appending ! to the names of functions with side-effects.

(defvar *!equivs* (make-hash-table))

(defun ry/! (fn)
  (or (gethash fn *!equivs*)
      fn))

(defun ry/def! (fn fn!)
  (setf (gethash fn *!equivs*)
        fn!))

(defun ry/memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
        (multiple-value-bind (val win) (gethash args cache)
          (if win
              val
              (setf (gethash args cache)
                    (apply fn args)))))))

(setq slowid (ry/memoize #'(lambda (x)
                             (sleep 5)
                             x)))

;; no cache
(time (funcall slowid 1))
;; cached id 1
(time (funcall slowid 1))
