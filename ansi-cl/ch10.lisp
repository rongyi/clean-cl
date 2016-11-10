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
