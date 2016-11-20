(defstruct node contents yes no)
(defvar *node* (make-hash-table))
(defun defnode (name conts &optional yes no)
  (setf (gethash name *node*)
        (make-node :contents conts
                   :yes yes
                   :no no)))

(defnode 'people "Is the person a man?" 'mail 'femail)
(defnode 'male "Is he living?" 'liveman 'deadman)
(defnode 'deadman "Was he American?" 'us 'them)
(defnode 'us "Is he on a coin?" 'coin 'cidence)
