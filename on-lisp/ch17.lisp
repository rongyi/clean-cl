;; The three big moments in a Lisp expression's life are:
;; read-time compile-time and runtime

;; The reader can be programmed at several levels. The easiest way to change its behavior is by defining new macro characters.

;; A macro character is a character which exacts special treatment from the Lisp reader.
;; e.g. '(' tells Lisp to begin reading a list.

;; Each such character has a function associated with it that tells the Lisp reader what to do when the
;; character is encountered.

;; You can change the function associated with an existing macro character, or define new macro characters of your own.
