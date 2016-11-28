;; It is inelegant to use a macro where a function would do
;; Most of the things we do with macros, we could not do with functions

;; It's a general principle of good design that if you find similar code appearing at several points in a program, you
;; should write a subroutine and replace the similar sequences of code with calls to the subroutine.

(defun ry/1+ (x)
  (+ 1 x))
(defmacro ry/1+ (x)
  `(+ 1 ,x))

(defmacro ry/while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

;; macro can do two things that functions can't:
;; they can control(or prevent) the evaluation of their arguments, and they are
;; expanded right into the calling context.

(defun ry/avg (&rest args)
  (/ (apply #'+ args)
     (length args)))
;; (ry/avg 1 2 3)
(defmacro ry/avg (&rest args)
  `(/ (+ ,@args)
      ,(length args)))
;; (ry/avg 1 2 3)

(defmacro ry/nil! (x)
  `(setf ,x nil))

(defmacro ry/defun (name params &body body)
  `(progn
     (setf (symbol-function ',name)
           #'(lambda ,params (block ,name ,@body)))
     ',name))
