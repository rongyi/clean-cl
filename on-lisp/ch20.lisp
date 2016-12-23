;; continuation
;; A continuation is a program frozen in action: a single functional object containing
;; the state of a computation. When the object is evaluated, the stored computation is
;; restarted where it left off.

;; We can think of the continuation at any given time as a function of one argument.

;; A closure is a function plus pointers to the lexical variables visible at the time it was created.

;; A continuation is a function plus a pointer to the whole stack pending at the time it was created.
;; When a continuation is evaluated, it returns a value using its own copy of the stack, ignoring the current one.
