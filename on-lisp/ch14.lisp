;; capture

(defmacro aif (test-form then-form &optional else-form)
  `(let ((it ,test-form))
     (if it ,then-form ,else-form)))

;; (aif t
;;      "hell"
;;      "mail")
