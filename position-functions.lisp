

(defclass board-position ()
  ((gameboard :initarg :gameboard
              :accessor gameboard)
   (to-move :initarg :to-move
            :initform 0
            :accessor to-move)
   (castling :initarg :castling
             :accessor castling)
   (en-passant :initarg :en-passant
               :initform nil
               :accessor en-passant)
   (white-pieces :initarg :white-pieces
                 :initform 0
                 :accessor white-pieces)
   (black-pieces :initarg :black-pieces
                 :initform 0
                 :accessor black-pieces)
   (pseudolegal-table :initarg :pseudolegal-table
                      :accessor pseudolegal-table)))

(defun make-board-position ())


 
(defmethod initialize-white-black-pieces :after ((pos board-position) &key)
  (let ((current-board (gameboard pos)))
    (dotimes (i 6)
      (setf (white-pieces pos) (logior (white-pieces pos) (aref current-board i))
            (black-pieces pos) (logior (black-pieces pos) (aref current-board (+ i 6)))))))
