;; The three big moments in a Lisp expression's life are:
;; read-time compile-time and runtime

;; The reader can be programmed at several levels. The easiest way to change its behavior is by defining new macro characters.

;; A macro character is a character which exacts special treatment from the Lisp reader.
;; e.g. '(' tells Lisp to begin reading a list.

;; Each such character has a function associated with it that tells the Lisp reader what to do when the
;; character is encountered.

;; You can change the function associated with an existing macro character, or define new macro characters of your own.

;; One of the oldest read-macros in Lisp is ', you could do without ' by always writing (quote a) instead of 'a

;; ' implementation
(set-macro-character #\'
                     #'(lambda (stream char)
                         (list 'quote (read stream t nil t))))

;; Macros get hold of the program when it has already been parsed into Lisp objects by the reader,
;; and read-macros operate on a program while it is still text.

(set-dispatch-macro-character #\# #\?
                              #'(lambda (stream cha1 char2)
                                  `#'(lambda (&rest ,(gensym))
                                       ,(read stream t nil t))))

;; (mapcar #?2 '(a b c))
;; (eq (funcall #?'a) 'a)
;; (eq (funcall #?#'oddp) (symbol-function 'oddp))

(set-macro-character #\] (get-macro-character #\) ))

(set-dispatch-macro-character #\# #\[
                              #'(lambda (stream cha1 cha2)
                                  (let ((accum nil)
                                        (pair (read-delimited-list #\] stream t)))
                                    (do ((i (ceiling (car pair)) (1+ i)))
                                        ((> i (floor (cadr pair)))
                                         (list 'quote (nreverse accum)))
                                      (push i accum)))))

;; #[2 7] expand to
;; 2 3 4 5 6 7

(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body)))

(let ((rpar (get-macro-character #\) )))
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left
                                  #'(lambda (stream char1 char2)
                                      (apply fn
                                             (read-delimited-list right stream t))))))

(defdelim #\[ #\] (x y)
  (list 'quote (mapa-b #'identity (ceiling x) (floor y))))

;; (let ((f1 (compose #'list #'1+))
;;       (f2 #'(lambda (x) (list (1+ x)))))
;;   (equal (funcall f1 7) (funcall f2 7)))

(defdelim #\{ #\} (&rest args)
  `(fn (compose ,@args)))

;; (funcall #{list 1+} 7)
