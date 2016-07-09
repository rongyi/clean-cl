;; dynamic create array
(setf arr (make-array '(2 3) :initial-element nil))

(setf (aref arr 0 0) 'b)
(aref arr 0 0)

;; literal array
#2a((b nil nil) (nil nil nil))

(setf vec (make-array 4 :initial-element nil))
(setf (aref vec 0) 'helloworld)

(vector "a" "b" 'c)
;; vector can also use aref to access the element
(let ((test (vector "a" "b" 'c)))
  (aref test 0))

;; also can use svref
(svref vec 0)

(defun bin-search (obj vec)
  (let ((len (length vec)))
    (and (not (zerop len))
         (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  (format t "~A~%" (subseq vec start (+ end 1)))
  (let ((range (- end start)))
    (if (zerop range)
        (if (eql obj (aref vec start))
            obj
            nil)
        (let ((mid (+ start (round (/ range 2)))))
          (let ((obj2 (aref vec mid)))
            (if (< obj obj2)
                (finder obj vec start (- mid 1))
                (if (> obj obj2)
                    (finder obj vec (+ mid 1) end)
                    obj)))))))

(bin-search 3 #(0 1 2 3 4 5 6 7 8 9))

(char-code #\c)
(mapcar 'char-code '(#\r #\x #\c))
(code-char 99)

(sort "I have a dream" 'char>)

(aref "hello rxc" 7)
;; eqal to aref
(char "hello rxc" 6)
(length "rxc")
(subseq "hello rxc" 6)

(let ((str (copy-seq "Merlin")))
  (setf (char str 3) #\k)
  str)

;; equal
(equal "hello" "hello")
(equal "hello" "Hello")
(string-equal "hello" "Hello")
(format nil "Hello ~A" "world")
(concatenate 'string "strong " "your" "self")

(elt '(1 2 3) (- (length '(1 2 3)) 1))

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (do ((forward 0 (+ forward 1))
              (back (- len 1) (- back 1)))
             ((or (> forward back)
                  (not (equal (elt s forward)
                            (elt s back))))
              (> forward back))))))
(mirror? "abba")

(position #\a "fantansia")
;; end means the first element to be not considered
(position #\a "fantansia" :start 4 :end 5)
(position 'a '((c d) (a b)) :key #'car)
(position '(a b) '((a b) (c d)) :test 'equal)

(position 3 '(1 0 -1 5) :test '<)

(defun second-word (str)
  (let ((p1 (+ (position #\  str) 1)))
    (subseq str p1 (position #\  str :start p1))))


(second-word "hello wolrd world")

(position-if 'oddp '(1 2 3 4 5))

(member 'a '(a b c d))
(member 'b '(a b c d))
(member-if 'numberp '(a b c 4))
(find #\a "cde")
(find-if 'characterp "1234ham")

(reduce 'intersection '((b r a d 'a) (b a d) (c a t)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\  ))))

(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if (lambda (c)
                                 (not (funcall test c)))
                               str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens str test p2)
                    nil)))
        nil)))

(tokens "ab12 3cde.f" 'constituent 0)

(tokens "ab12 3cde.f
gh" 'constituent 0)


(defun parse-date (str)
  (let ((toks (tokens str 'constituent 0)))
    (list (parse-integer (car toks))
          (parse-month (second toks))
          (parse-integer (third toks)))))

;; TODO: add more month here
(defconstant month-names
  #("jan" "feb"))

(defun parse-month (str)
  (let ((p (position str month-names :test 'string-equal)))
    (if p
        (+ p 1)
        nil)))

(parse-date "16 jan 1988")

(defun ry/parse-integer (str)
  (if (every 'digit-char-p str)
      (let ((accum 0))
        (dotimes (pos (length str))
          (setf accum (+ (* accum 10)
                         (digit-char-p (char str pos)))))
        accum)
      nil))

(ry/parse-integer "100203")
