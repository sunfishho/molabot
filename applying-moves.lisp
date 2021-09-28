

(defun get-move-origin (move)
  (logand move 63))

(defun get-move-destination (move)
  (logand (ash move -6) 63))

(defun get-move-promotion (move)
  (logand (ash move -12) 3))

(defun get-move-type (move)
  (logand (ash move -14) 3))

(defmacro set-bitboard-index-zero (bboard index)
  `(and-this ,bboard (lognot (ash 1 ,index))))

(defmacro set-bitboard-index-one (bboard index)
  `(or-this ,bboard (ash 1 ,index)))

(defmacro toggle-piece-position-hash (gshash piecetype loc)
  `(xor-this (getf ,gshash :hash-value) (aref *zobrist-piece-keys* ,piecetype ,loc)))

;;Helper function for apply-move, dealing with moves that are not en passants, promotions, or castles
(defun apply-normal-move (gshash origin destination origin-piece-type destination-piece-type)
  (let ((gameboard (getf gshash :gameboard)))
    (set-bitboard-index-one (aref gameboard origin-piece-type) destination)
    (toggle-piece-position-hash gshash origin-piece-type destination)
    (when (not (eql destination-piece-type -1))
      (set-bitboard-index-zero (aref gameboard destination-piece-type) destination)
      (toggle-piece-position-hash gshash destination-piece-type destination))
    (when (not (eql (getf gshash :en-passant) -1))
      (xor-this (getf gshash :hash-value) (aref *zobrist-en-passant-keys* (get-col-from-square (getf gshash :en-passant)))))
    (setf (getf gshash :en-passant) (if (and (or (eql origin-piece-type 0) (eql origin-piece-type 6))
                                                 (eql (abs (- origin destination)) 16))
                                            (/ (+ origin destination) 2)
                                            -1))
    (when (not (eql (getf gshash :en-passant) -1))
      (xor-this (getf gshash :hash-value) (aref *zobrist-en-passant-keys* (get-col-from-square (getf gshash :en-passant)))))))

;;Helper function for apply-move, dealing with functions that are promotions (or underpromotions)
(defun apply-promotion-move (gshash move destination destination-piece-type color)
  (let ((gameboard (getf gshash :gameboard)))
    (when (not (eql destination-piece-type -1))
      (toggle-piece-position-hash gshash destination-piece-type destination)
      (set-bitboard-index-zero (aref gameboard destination-piece-type) destination))
    (let ((piecename (cond ((eql (get-move-promotion move) 0)
                            (if (eql color 0) #\Q #\q))
                           ((eql (get-move-promotion move) 1)
                            (if (eql color 0) #\N #\n))
                           ((eql (get-move-promotion move) 2)
                            (if (eql color 0) #\R #\r))
                           (t
                            (if (eql color 0) #\B #\b)))))
      (toggle-piece-position-hash gshash (piece-to-index piecename) destination)
      (set-bitboard-index-one (aref gameboard (piece-to-index piecename)) destination))
    (when (not (eql (getf gshash :en-passant) -1))
      (xor-this (getf gshash :hash-value) (aref *zobrist-en-passant-keys* (get-col-from-square (getf gshash :en-passant)))))
    (setf (getf gshash :en-passant) -1)))

;;Helper function for apply-move, dealing with en passants
(defun apply-en-passant-move (gshash destination origin-piece-type )
  (let ((gameboard (getf gshash :gameboard)))
    (toggle-piece-position-hash gshash origin-piece-type destination)
    (set-bitboard-index-one (aref gameboard origin-piece-type) destination)
    (toggle-piece-position-hash gshash (if (eql origin-piece-type 0) 6 0) (- destination (if (eql origin-piece-type 0) -8 8)))
    (set-bitboard-index-zero (aref gameboard (if (eql origin-piece-type 0) 6 0)) (- destination (if (eql origin-piece-type 0) -8 8)))
    ;;Technically this check should not be needed given we just played an en passant, but it doesn't hurt.
    (when (not (eql (getf gshash :en-passant) -1))
      (xor-this (getf gshash :hash-value) (aref *zobrist-en-passant-keys* (get-col-from-square (getf gshash :en-passant)))))
    (setf (getf gshash :en-passant) -1)))

;;;Obviously, need to update this when castling/promotion are implemented
;;;This takes the game-state and a move and applies the move.
(defun apply-move (gshash move)
  (let* ((gameboard (getf gshash :gameboard))
         (origin (get-move-origin move))
         (destination (get-move-destination move))
         (move-type (get-move-type move))
         (origin-piece-type (find-piece-type gameboard origin))
         (destination-piece-type (find-piece-type gameboard destination))
         (en-passant-rights (getf gshash :en-passant))
         (castling-rights (getf gshash :castling))
         (color (getf gshash :to-move)))
    (xor-this (getf gshash :to-move) 1)
    (set-bitboard-index-zero (aref gameboard origin-piece-type) origin)
    (xor-this (getf gshash :hash-value)
              (aref *zobrist-piece-keys* origin-piece-type origin)
              *zobrist-to-move-key*)
    (cond ((eql move-type 0)
           (apply-normal-move gshash origin destination origin-piece-type destination-piece-type)
           (return-from apply-move (values destination-piece-type en-passant-rights castling-rights)))
          ((eql move-type 1)
           (apply-promotion-move gshash move destination destination-piece-type color)
           (return-from apply-move (values destination-piece-type en-passant-rights castling-rights)))
          ((eql move-type 2)
           (apply-en-passant-move gshash destination origin-piece-type)
           (return-from apply-move (values destination-piece-type en-passant-rights castling-rights))))))



;;Helper function for undo-move, dealing with moves that are not en passants, promotions, or castles.
(defun undo-normal-move (gshash origin destination origin-piece-type destination-piece-type)
  (let ((gameboard (getf gshash :gameboard)))
    ;;Put the original move back on the origin
    (toggle-piece-position-hash gshash origin-piece-type origin)
    (set-bitboard-index-one (aref gameboard origin-piece-type) origin)
    (when (not (eql destination-piece-type -1))
      ;;If there was a piece captured, put it back
      (toggle-piece-position-hash gshash destination-piece-type destination)
      (set-bitboard-index-one (aref gameboard destination-piece-type) destination))))

;;Helper function for undo-move, dealing with moves that are promotions
(defun undo-promotion-move (gshash move origin destination destination-piece-type)
  (let ((gameboard (getf gshash :gameboard))
        (color (getf gshash :to-move)))
    ;;Put a pawn back where the old pawn that just promoted was
    (toggle-piece-position-hash gshash (piece-to-index (if (eql color 0) #\P #\p)) origin)
    (set-bitboard-index-one (aref gameboard (piece-to-index (if (eql color 0) #\P #\p))) origin)
    (when (not (eql destination-piece-type -1))
      ;;If a piece was captured, put it back.
      (toggle-piece-position-hash gshash destination-piece-type destination)
      (set-bitboard-index-one (aref gameboard destination-piece-type) destination))
    (let ((piecename (cond ((eql (get-move-promotion move) 0)
                            (if (eql color 0) #\Q #\q))
                           ((eql (get-move-promotion move) 1)
                            (if (eql color 0) #\N #\n))
                           ((eql (get-move-promotion move) 2)
                            (if (eql color 0) #\R #\r))
                           (t
                            (if (eql color 0) #\B #\b)))))
      ;;Now, we remove the piece the pawn promoted to.
      (toggle-piece-position-hash gshash (piece-to-index piecename) destination)
      (set-bitboard-index-zero (aref gameboard (piece-to-index piecename)) destination))))

;;Helper function for undo-move, dealing with en passant moves.
(defun undo-en-passant-move (gshash origin destination origin-piece-type)
  (let ((gameboard (getf gshash :gameboard)))
    ;;Put the pawn back on the origin
    (toggle-piece-position-hash gshash origin-piece-type origin)
    (set-bitboard-index-one (aref gameboard origin-piece-type) origin)
    ;;Add the pawn that got captured back
    (toggle-piece-position-hash gshash (if (eql origin-piece-type 0) 6 0) (- destination (if (eql origin-piece-type 0) -8 8)))
    (set-bitboard-index-one (aref gameboard (if (eql origin-piece-type 0) 6 0)) (- destination (if (eql origin-piece-type 0) -8 8)))))

;;;This undos a move, and requires the en-passant-rights and castling-rights
(defun undo-move (gshash move destination-piece-type en-passant-rights castling-rights)
  (let* ((gameboard (getf gshash :gameboard))
         (origin (get-move-origin move))
         (destination (get-move-destination move))
         (move-type (get-move-type move))
         (origin-piece-type (find-piece-type gameboard destination)))
    ;;XOR out the old en-passant value
    (when (not (eql (getf gshash :en-passant) -1))
      (xor-this (getf gshash :hash-value) (aref *zobrist-en-passant-keys* (get-col-from-square (getf gshash :en-passant)))))
    ;;Change the hash value to match who is now to move
    (xor-this (getf gshash :hash-value) *zobrist-to-move-key*)
    (xor-this (getf gshash :to-move) 1)
    (setf (getf gshash :en-passant) en-passant-rights
          (getf gshash :castling) castling-rights)
    ;;XOR hash value with the new en-passant value
    (when (not (eql (getf gshash :en-passant) -1))
      (xor-this (getf gshash :hash-value) (aref *zobrist-en-passant-keys* (get-col-from-square (getf gshash :en-passant)))))
    ;;XOR hash value with the value for the piece that's being moved back to the origin.
    (when (not (eql move-type 1))
      (toggle-piece-position-hash gshash origin-piece-type destination))
    (set-bitboard-index-zero (aref gameboard origin-piece-type) destination)
    (cond ((eql move-type 0)
           (undo-normal-move gshash origin destination origin-piece-type destination-piece-type))
          ((eql move-type 1)
           (undo-promotion-move gshash move origin destination destination-piece-type))
          ((eql move-type 2)
           (undo-en-passant-move gshash origin destination origin-piece-type)))))

(defun generate-sorted-moves (gshash)
  (let* ((legal-moves (generate-all-legal-moves gshash))
         (num-legal-moves (length legal-moves))
         (legal-moves-eval (make-array (list num-legal-moves)))
         (nil-eval (if (eql (getf gshash :to-move) 0) -1000000 1000000)))
    (dotimes (i num-legal-moves)
      (let ((move (aref legal-moves i)))
        (multiple-value-bind (destination-piece-type en-passant-rights castling-rights)
            (apply-move gshash (aref legal-moves i))
          (let ((hashval (gethash (getf gshash :hash-value) *transposition-table*)))
            (setf (aref legal-moves-eval i) (list move (if (and hashval (eql (nth 3 hashval) 0)) (nth 1 hashval) nil-eval))))
          (undo-move gshash move destination-piece-type en-passant-rights castling-rights))))
    (sort legal-moves-eval (if (eql (getf gshash :to-move) 0) #'> #'<) :key #'cadr)
    (map 'vector #'car legal-moves-eval)))
