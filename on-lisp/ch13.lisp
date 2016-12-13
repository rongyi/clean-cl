
(defun avg (&rest args)
  (/ (apply #'+ args) (length args)))

;; (avg 1 2 3)

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))
;; (avg 1 2 3)

(defun most-of (&rest args)
  (let ((all 0)
        (hits 0))
    (dolist (a args)
      (incf all)
      (if a (incf hits)))
    (> hits (/ all 2))))

;; (most-of t t t nil)

(defmacro most-of (&rest args)
  (let ((need (floor (/ (length args) 2)))
        (hits (gensym)))
    `(let ((,hits 0))
       (or ,@(mapcar #'(lambda (a)
                         `(and ,a (> (incf ,hits) ,need)))
                     args)))))

;; (most-of t t t nil)
