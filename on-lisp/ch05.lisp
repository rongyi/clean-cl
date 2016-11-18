
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

;; example
;; (funcall (compose #'1+ #'find-if) #'oddp '(2 3 4))
;; define complement
;; (defun ry/complement (pred)
;;   (compose #'not pred))
;; (remove-if (ry/complement #'oddp) '(1 2 3))

(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else
              (funcall else x)))))

;; means function intersection
(defun fint(fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
        #'(lambda (x)
            (and (funcall fn x)
                 (funcall chain x))))))

;; (remove-if (fint #'(lambda (x)
;;                      (> x 0))
;;                  #'(lambda (x)
;;                      (< x 10))) '(1 2 3 10 11 23))

(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
        #'(lambda (x)
            (or (funcall fn x)
                (funcall chain x))))))

;; (remove-if (fun #'(lambda (x)
;;                      (> x 20))
;;                  #'(lambda (x)
;;                      (< x 10))) '(1 2 3 10 11 23))
;; ==> '(10 11)

;; recursive section
(defun ry/length (lst)
  (if (null lst)
      0
      (1+ (ry/length (cdr lst)))))
;; (ry/length '(1 2 3 4))

(defun ry/every (fn lst)
  (if (null lst)
      t
      (and (funcall fn (car lst))
           (ry/every fn (cdr lst)))))

;; (ry/every #'evenp '(2 4 6))
;; (ry/every #'evenp '(2 4 7))
