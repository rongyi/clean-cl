(defparameter *nodes* '((living-room (you are in the living-room.
                                      a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden.
                                 there is a well in front of you.))
                        (attic (you are in the attic.
                                there is a giant welding torch in the corner.))))

(defparameter *edges* '((living-room (garden west door)
                         (attic upstairs ladder))
                        (garden (living-room east door))
                        (attic (living-room downstairs ladder))))

(defparameter *objects* '(whishkey bucket frog chain))

(defparameter *object-locations* '((whishkey living-room)
                                   (bucket living-room)
                                   (chain garden)
                                   (frog garden)))

(defparameter *location* 'living-room)

;; copy from the book
;; Why don’t we just reference the *nodes* variable directly from the describe-
;; location function? Because this function is written in the functional programming
;; style. In this style, a function will reference only parameters or variables
;; declared in the function itself, and it will do nothing besides return a value,
;; which is the description of the location in this case.
;; By writing functions that don’t reference variables in the “outside world”
;; directly and that don’t perform any actions other than returning a value, you
;; can write code that can easily be tested in isolation. You should try to write
;; your Lisp functions in this style whenever possible.
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defun ry/describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))
(defun describe-paths (location edges)
  (apply #'append (mapcar #'ry/describe-path  (cdr (assoc location edges)))))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))

(defun describe-objects (loc objs obj-locs)
  (labels ((describe-obj (obj)
             `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locs)))))


(defun look ()
  (append (describe-location *location* *nodes*)
          (describe-paths *location* *edges*)
          (describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
                    (cdr (assoc *location* *edges*))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you can not go that way.))))

(defun pickup (object)
  (cond ((member object
                 (objects-at *location* *objects* *object-locations*))
         (push (list object 'body) *object-locations*)
         `(you are now carrying the ,object))
        (t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;; (print "foo")
;; (progn (print "this")
;;        (print "is")
;;        (print "a")
;;        (print "test"))

;; (progn (prin1 "this")
;;        (prin1 "is")
;;        (prin1 "a")
;;        (prin1 "test"))

(defun say-hello ()
  (princ "Please type your name:")
  (let ((name (read)))
    (princ "Nice to meet you , ")
    (princ name)))

;; (print '3)
;; (print '3.4)
;; (print 'foo)
;; (print '"foo")
;; (print '#\a)

;; for human readable
;; (princ '3)
;; (princ '3.4)

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))


(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eq item #\") (tweak-text rest caps (not lit)))
            (lit (cons item (tweak-text rest nil lit)))
            ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
            (t (cons (char-downcase item) (tweak-text rest nil nil)))))))
(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))
