(defmacro nil! (var)
  (list 'setq var nil))

(let ((n 10))
  (nil! n))
(macroexpand-1 '(nil! n))

;; Here's the stroke of genius: the output of the parse consists of
;; lists of Lisp Objects. With macros, we can manipulate the program
;; while it's in this intermediate from between parse and compiler.
