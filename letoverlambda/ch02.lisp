;; 这个函数只是在一个stream中找到target，找到一次之后就结束了，
;; 后面出现不出现已经不重要了，因为这个函数始终会返回T， shit
(defun block-scanner (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
         (curr trig))
    (lambda (data-string)
      (let ((data (coerce data-string 'list))
            (ret nil))
        (dolist (c data)
          (if curr
              (setq curr
                    (if (char= (car curr) c)
                        (cdr curr)
                        trig))))
        (not curr)))))

(defvar scanner (block-scanner "jihad"))
(funcall scanner "jiha")
(funcall scanner "d")
(funcall scanner "jiha")
(funcall scanner "jihad")
(funcall scanner "jihad")
(funcall scanner "we will ji")
(funcall scanner "had")
(funcall scanner "jihad")
(funcall scanner "jihajihad")
