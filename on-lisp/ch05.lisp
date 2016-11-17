
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
;; (time (funcall slowid 1))
;; cached id 1
;; (time (funcall slowid 1))

;; (find-if #'oddp '(2 3 4))

;; all the functions given as arguments to compose must be functions of one argument, except the last
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
      #'identity))

;; test reduce
;; (reduce #'list '(1 2 3 4) :from-end t :initial-value 0)
;; (reduce #'funcall '(1+) :from-end t :initial-value 1)
