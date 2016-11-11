(eval '(+ 1 2 3))
(defmacro setxnil (x)
  (list 'set x nil))

(macroexpand-1 '(setxnil y))

(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defun quicksort (vec l r)
  (let ((i l)
        (j r)
        (p (svref vec (round (+ l r) 2))))
    (while (<= i j)
      (while (< (svref vec i) p)
        (incf i))
      (while (> (svref vec j) p)
        (decf j))
      (when (<= i j)
        (rotatef (svref vec i)
                 (svref vec j))
        (incf i)
        (decf j)))
    (if (> (- j l) 1)
        (quicksort vec 1 j))
    (if (> (- r i) 1)
        (quicksort vec i r)))
  vec)

;; (quicksort (vector 1 9 2 4 5) 0 4)

;; when you start writing macros, you have to start thinking like a language designer

(defmacro ntimes (n &rest body)
  (let ((g (gensym))
        (h (gensym)))
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1)))
           ((>= ,g ,h))
         ,@body))))

(ntimes 3
        (princ "="))

(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
         ((> ,var ,gstop))
       ,@body)))

;; (for i 1 7
;;   (princ i))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

;; (in 'a 'b 'c 'd 'a)
;; (pprint  (macroexpand-1 '(in 'a 'b 'c 'd)))

(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
         (mapcar #'(lambda (expr)
                   `(,(incf key) ,expr))
                 exprs))))

;; (pprint (macroexpand-1 '(random-choice (princ 1) (princ 2))))
;; (random-choice (princ 1) (princ 2))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

;; (avg 1 2 3 4)

(defmacro ry/with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it
         ,then
         ,else)))
