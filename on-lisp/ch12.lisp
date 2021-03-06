;; (setq lst '(a b c))
;; (setf (car lst) 480)

;; (progn (rplaca lst 480) 480)
;; This transformation from query to assertion is called *inversion*
;; All the most frequently used Common Lisp access functions have predefined inversions,
;; including car, cdr, nth, aref, get, gethash, and the access functions created by defstruct


;; An expression which can serve as the first arguments to setf is called a /generlized variable/.
;; generlized variable have turned out to be a powerful abstraction.


;; Wrong version
(defmacro toggle (obj)
  `(setf ,obj (not ,obj)))

(let ((lst '(a b c)))
  (toggle (car lst))
  lst)
(testmacro (toggle (car '(a b c))))

;; Generalized variable are like a health food that tastes good.
;; The yield programs which are virtuously modular, and yet beautifully elegant.

;; (toggle (nth (incf 0) lst))
;; This increments i twice, and sets the (i + 1)th element to the opposite of the
;; (i + 2)th element.
(let ((lst '(t nil t))
      (i -1))
  (toggle (nth (incf i) lst))
  lst)

(define-modify-macro toggle () not)
(let ((lst '(t nil t))
      (i -1))
  (toggle (nth (incf i) lst))
  lst)

(defmacro allf (val &rest args)
  (let ((gval (gensym)))
    `(let ((,gval ,val))
       (setf ,@(mapcan #'(lambda (a) (list a gval))
                       args)))))


(defmacro nilf (&rest args)
  `(allf nil ,@args))
(defmacro tf (&rest args)
  `(allf t ,@args))

(defmacro toggle (&rest args)
  `(progn
     ,@(mapcar #'(lambda (a)
                   `(toggle2 ,a))
               args)))

(define-modify-macro toggle2 () not)

;; mapcan can be see as a glue
;; (mapcan #'(lambda (x)
;;             (if (oddp x)
;;                 (list x)))
;;         '(1 2 3 4 5))

;; (setf x 1 y 2)
;; (setf x nil y nil z nil)
;; (nilf x y z)
;; (testmacro  (nilf x y z))
;; (nilf x y z)


;; Since we can have the generality of setf at no extra cost, it is rarely desireable to use setq in a macroexpansion

(define-modify-macro concf (obj) nconc)
(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))
(define-modify-macro concnew (obj &rest args)
  (lambda (place obj &rest args)
    (unless (apply #'member obj place args)
      (nconc place (list obj)))))
;; based on function conc1
(define-modify-macro conc1f (obj) conc1)

;; write a incf like function _f
;; wrong version
(defmacro _f (op place &rest args)
  `(setf ,place (,op ,place ,@args)))

;; (aref (setq alpha (make-array 4)) 3)
;; (setf (aref alpha 3) 'sirens)

;; test the return of the get-setf-method function
(let ((a (make-array 4))
      (i -1))
  (setf (aref a 0) 1)
  (incf (aref a (incf i)))
  (get-setf-method '(aref a (incf i))))

(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-method place)
    `(let* (,@(mapcar #'list vars forms)
            (,(car var) (,op ,access ,@args)))
       ,set)))

(defmacro conc1f (lst obj)
  `(_f nconc ,lst (list ,obj)))

;; (let ((x '(a b c d)))
;;   (conc1f x 'd))

(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-method place)
      (let ((g (gensym)))
        `(let* ((,g ,obj)
                ,@(mapcar #'list vars forms)
                (,(car var) (delete ,g ,access ,@args)))
           ,set))))

;; (setq x '(1 2 (a b) 3))
;; (pull 2 x)
;; (pull '(a b) x :test #'equal)

(defmacro pull-if (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-method place)
      (let ((g (gensym)))
        `(let* ((,g ,obj)
                ,@(mapcar #'list vars forms)
                (,(car var) (delete-if ,g ,access ,@args)))
           ,set))))

;; (let ((lst '(1 2 3 4 5 6 7)))
;;   (pull-if #'oddp lst)
;;   lst)

(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access)
      (get-setf-method place)
    (ry/with-gensyms (gn glst)
      `(let* ((,gn ,n)
              ,@(mapcar #'list vars forms)
              (,glst ,access)
              (,(car var) (nthcdr ,gn ,glst)))
         (prog1 (subseq ,glst 0 ,gn)
           ,set)))))

;; (setq x '(a b c d e f))
;; (popn 3 x)

(defmacro sortf (op &rest places)
  (let* ((meths (mapcar #'(lambda (p)
                            (multiple-value-list
                             (get-setf-method p)))
                        places))
         (temps (apply #'append (mapcar #'third meths))))
    `(let* ,(mapcar #'list
                    (mapcan #'(lambda (m)
                                (append (first m)
                                        (third m)))
                            meths)
                    (mapcan #'(lambda (m)
                                (append (second m)
                                        (list (fifth m))))
                            meths))
       ,@(mapcon #'(lambda (rest)
                     (mapcar #'(lambda (arg)
                                 `(unless (,op ,(car rest) ,arg)
                                    (rotatef ,(car rest) ,arg)))
                             (cdr rest)))
                 temps)
       ,@(mapcar #'fourth meths))))

;; (setq x 1 y 2 z 3)
;; (testmacro (sortf > x y z))
;; (sortf > x y z)
;; (list x y z)

(defvar *cache* (make-hash-table))
(defvar *world* '((a . 2) (b . 16)))
(defun retrieve (key)
  (multiple-value-bind (x y) (gethash key *cache*)
    (if y
        (values x y)
        (cdr (assoc key *world*)))))

(defsetf retrieve (key) (val)
  `(setf (gethash ,key *cache*) ,val))

(retrieve 'a)
(setf (retrieve 'a) 22)
