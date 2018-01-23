(defparameter *small* 1)
(defparameter *big* 100)
(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

(defun smaller()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun start-over ()
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess-my-number))

;; (let ((a 10)
;;       (b 20))
;;   (+ a b))
;; (flet ((f ()
;;          1)
;;        (g ()
;;          2))
;;   (+ (f) (g)))

;; (labels ((f ()
;;            1)
;;          (g ()
;;            (+ 3 (f))))
;;   (g))

;; (eq 'foo 'foo)
;; (expt 2 3)
;; (/ 10 3)
;; (* (/ 1 3) 3)
;; (/ 4.0 6)
;; (princ "Tutti Frutti")

(cons (cons 'peas (cons 'carrots (cons 'tomatoes ())))
      (cons (cons 'pork (cons 'beef (cons 'chicken ()))) ()))

(let ((a '((peas carrots tomatoes) (pork beef chicken) duck)))
  (cadadr a))

(if '(1)
    'i-am-true
    'i-am-false)

(defun my-length (lst)
  (if lst
      (1+ (my-length (cdr lst)))
      0))

;; (my-length '(list with four symbols))
;; (eq '() nil)
;; (eq '() ())
;; (eq '() 'nil)

(if (= (+ 1 2) 4)
    'yup
    'nope)
(if '()
    'the-list-has-stuff-in-it
    'the-list-is-empty)

(if (oddp 5)
    'odd-number
    'even-number)

(if (oddp 5)
    'odd-number
    (/ 1 0))

(defvar *number-was-odd* nil)
(if (oddp 5)
    (progn (setf *number-was-odd* t)
           'odd-number)
    'even-number)

(defvar *number-is-odd* nil)
(when (oddp 5)
  (setf *number-is-odd* t)
  'odd-number)

(unless (oddp 4)
  (setf *number-is-odd* nil)
  'even-number)

(defvar *arch-enemy* nil)
(defun pudding-eater (person)
  (cond ((eq person 'henry) (setf *arch-enemy* 'stupid-lisp-alien) '(curse you lisp alien - you ate my pudding))
        ((eq person 'johnny) (setf *arch-enemy* 'useless-old-johnny) '(i hope you choked on my pudding johnny))
        (t )))

(defun pudding-eater2 (person)
  (case person
    ((henry) (setf *arch-enemy* 'stupid-lisp-alien) '(curse you lisp alien - you ate my pudding))
    ((johnny) (setf *arch-enemy* 'useless-old-johnny) '(i hope you choked on my pudding johnny))
    (otherwise '(why you eat my pudding stranger ?))))

(find-if #'oddp '(2 4 5 7))

;; equal eql eq = string-equal equalp
;; 1. use eq to compare symbols
;; 2. use equal for everything else

;; eq for symbols
(defparameter *fruit* 'apple)
(cond ((eq *fruit* 'apple) 'its-an-apple)
      (eq *fruit* 'orange) 'its-an-orange)

;; equal for looking alike comparing
(equal 'apple 'apple)
(equal (list 1 2 3) (list 1 2 3))
(equal '(1 2 3) (cons 1 (cons 2 (cons 3 nil))))
(equal 5 5)
(equal 2.5 2.5)
(equal "foo" "foo")
(equal #\a #\a)

;; eql like eq but with number and char
(eql 'foo 'foo)
(eql 3.4 3.4)
(eql #\a #\a)

;; equalp: extra with equal string ignore case compare, int compare float
(equalp "Bob Smith" "bob smith")
(equalp 0 0.0)
