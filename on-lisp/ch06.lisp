(defstruct node contents yes no)
(defvar *nodes* (make-hash-table))
(defun defnode (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (make-node :contents conts
                   :yes yes
                   :no no)))

(defnode 'people "Is the person a man?" 'male 'female)
(defnode 'male "Is he living?" 'liveman 'deadman)
(defnode 'deadman "Was he American?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'cidence)
(defnode 'coin "Is the coin a penny?" 'penny 'coins)
(defnode 'penny 'lincoln)

(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (cond ((node-yes n)
           (format t "~A~%>> " (node-contents n))
           (case (read)
             (yes (run-node (node-yes n)))
             (t (run-node (node-no n)))))
          (t (node-contents n)))))

;; this version is a closure version
(defun defnodev2 (name conts &optional yes no)
  (setf (gethash name *nodes*)
        (if yes
            #'(lambda ()
                (format t "~A~%>> " conts)
                (case (read)
                  (yes (funcall (gethash yes *nodes*)))
                  (t (funcall (gethash no *nodes*)))))
            #'(lambda () conts))))

(defnodev2 'people "Is the person a man?" 'male 'female)
(defnodev2 'male "Is he living?" 'liveman 'deadman)
(defnodev2 'deadman "Was he American?" 'us 'them)
(defnodev2 'us "Is he on a coin?" 'coin 'cidence)
(defnodev2 'coin "Is the coin a penny?" 'penny 'coins)
(defnodev2 'penny 'lincoln)
