;; macro define macro
;; Fortunately, in a language like Lisp you don't have to live with all the decisions of the designers.


;; (defmacro dbind (&rest args)
;;   `(destructuring-bind ,@args))

;; (defmacro mvbind (&rest args)
;;   `(multiple-value-bind ,@args))

;; Nested backquotes are notoriously hard to understand
;; 嵌套backquote太难懂了

(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))

;; (testmacro (abbrev mvbind multiple-value-bind))

(defmacro abbrevs (&rest names)
  `(progn
     ,@(mapcar #'(lambda (pair)
                   `(abbrev ,@pair))
               (group names 2))))

;; (abbrevs mvbind multiple-value-bind
;;          mvsetq multiple-value-setq)
