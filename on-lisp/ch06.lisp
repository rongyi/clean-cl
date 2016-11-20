(defstruct node contents yes no)
(defvar *node* (make-hash-table))
(defun defnode (name conts &optional yes no)
  (setf (gethash name *node*)
        (make-node :contents conts
                   :yes yes
                   :no no)))
