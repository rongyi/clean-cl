
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
;; (testmacro (most-of t t t nil))

;; under this situation macro is far better than function
(defun nthmost (n lst)
  (nth n (sort (copy-list lst) #'>)))

;; (nthmost 2 '(2 6 1 5 3 4))
(defmacro nthmost (n lst)
  (if (and (integerp n) (< n 20))
      (ry/with-gensyms (glst gi)
        (let ((syms (map0-n #'(lambda (x)
                                (gensym))
                            n)))
          `(let ((,glst ,lst))
             (unless (< (length ,glst) ,(1+ n))
               ,@(gen-start glst syms)
               (dolist (,gi ,glst)
                 ,(nthmost-gen gi syms t))
               ,(car (last syms))))))
      `(nth ,n (sort (copy-list ,lst) #'>))))

(nthmost 2 '(2 6 1 5 3 4))

;; (testmacro (nthmost 2 '(2 6 1 5 3 4)))

(defun gen-start (glst syms)
  (reverse
   (maplist #'(lambda (syms)
                (let ((var (gensym)))
                  `(let ((,var (pop ,glst)))
                     ,(nthmost-gen var (reverse syms)))))
            (reverse syms))))

(defun nthmost-gen (var vars &optional long?)
  (if (null vars)
      nil
      (let ((else (nthmost-gen var (cdr vars) long?)))
        (if (and (not long?) (null else))
            `(setq ,(car vars) ,var)
            `(if (> ,var ,(car vars))
                 (setq ,@(mapcan #'list
                                 (reverse vars)
                                 (cdr (reverse vars)))
                       ,(car vars) ,var)
                 ,else)))))
