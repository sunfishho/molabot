;;;This file should contain the functions to:
;; 1. get a 8x8 board from a FEN
;; 2. get a bitboard from a 8x8 board and vice versa

(defvar *starting-fen* "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))


(defmacro or-this (var &rest values)
  `(setf ,var (logior ,var ,@values)))

(defmacro and-this (var &rest values)
  `(setf ,var (logand ,var ,@values)))

(defmacro xor-this (var &rest values)
  `(setf ,var (logxor ,var ,@values)))


;;;This returns the floor of log base 2 of power-of-two
(defun log2 (power-of-two)
  (when (< power-of-two 1) (error "Log2 out of bounds"))
  (do ((x 0 (+ x 1))
       (val power-of-two (ash val -1)))
      ((eql val 1) x)))

;;;returns least significant bit of x
(defun lsb (x)
  (logand x (- 0 x)))

(defun get-row-from-square (index)
  (ash index -3))

(defun get-col-from-square (index)
  (logand index 7))

(defun castling-perms-to-index (castling)
  (position castling '("-" "K" "Q" "k" "q" "KQ" "Kk" "Kq" "Qk" "Qq" "kq" "KQk" "KQq" "Kkq" "Qkq" "KQkq") :test #'equal))

(defvar *zobrist-piece-keys* (make-array '(12 64)))
(defvar *zobrist-to-move-key* 0)
(defvar *zobrist-en-passant-keys* (make-array '(8)))
(defvar *zobrist-castling-rights* (make-array '(16)))

(defun initialize-zobrist-keys ()
  (dotimes (i 12)
    (dotimes (j 64)
      (setf (aref *zobrist-piece-keys* i j) (random 18446744073709551615))))
  (setf *zobrist-to-move-key* (random 18446744073709551615))
  (dotimes (i 8)
    (setf (aref *zobrist-en-passant-keys* i) (random 18446744073709551615)))
  (dotimes (i 16)
    (setf (aref *zobrist-castling-rights* i) (random 18446744073709551615))))

(defun get-zobrist-hash (game-state)
  (let ((hash 0))
    (dotimes (piece 12)
      (let ((loc-pieces (aref (getf game-state :gameboard) piece)))
        (while (not (eql loc-pieces 0))
               (xor-this hash (aref *zobrist-piece-keys* piece (log2 (lsb loc-pieces))))
               (and-this loc-pieces (- loc-pieces 1)))))
    (when (eql (getf game-state :to-move) 1)
      (xor-this hash *zobrist-to-move-key*))
    (when (not (eql (getf game-state :en-passant) -1))
      (xor-this hash (aref *zobrist-en-passant-keys* (get-col-from-square (getf game-state :en-passant)))))
    (xor-this hash (aref *zobrist-castling-rights* (castling-perms-to-index (getf game-state :castling))))))


(defun rowcol-to-u64-index (row col)
  (+ (* 8 row) col))

(defun piece-to-index (char-piece)
  (let ((piece-ordering "PNBRKQpnbrkq"))
    (position char-piece piece-ordering)))

;;;Converts a bitboard into a list of squares that are occupied
(defun u64-to-list-of-squares (u64)
  (if (eql u64 0)
      nil
      (cons (log2 (lsb u64)) (u64-to-list-of-squares (logxor u64 (lsb u64))))))

;;;This converts the 8x8 board into an array size 8, PNBRKQpnbrkq
(defun board2d-to-gameboard (board2d)
  (let ((gameboard (make-array '(12) :element-type '(unsigned-byte 64))))
    (dolist (row '(0 1 2 3 4 5 6 7))
      (dolist (col '(0 1 2 3 4 5 6 7))
        (if (aref board2d row col)
            (setf (aref gameboard (piece-to-index (aref board2d row col)))
                  (coerce (+ (ash 1 (rowcol-to-u64-index row col)) (aref gameboard (piece-to-index (aref board2d row col)))) '(unsigned-byte 64))))))
    gameboard))

;;;This converts the combination of the bitboards into the 8x8 representation.
(defun gameboard-to-board2d (gameboard)
  (let ((board2d (make-array '(8 8) :initial-element nil))
        (piecelist "PNBRKQpnbrkq"))
    (dolist (piecetype '(0 1 2 3 4 5 6 7 8 9 10 11))
      (let ((piece-char (char piecelist piecetype)))
        (dolist (loc (u64-to-list-of-squares (aref gameboard piecetype)))
          (setf (aref board2d (ash loc -3) (logand loc 7)) piece-char)) ))
    board2d))

;;;Given a single bitboard
(defun bitboard-to-board2d (bboard)
  (let ((board2d (make-array '(8 8) :initial-element 0)))
    (dolist (loc (u64-to-list-of-squares bboard))
      (setf (aref board2d (ash loc -3) (logand loc 7)) 1))
    board2d))

;;;return the 8x8 board array given a fen
(defun fen-to-pieces (fen)
  (let ((parts-of-fen (split-sequence:SPLIT-SEQUENCE #\space fen)))
    parts-of-fen))


;;;helper function for get-board-from-fen-string
(defun expand-fen-row (unexpanded-row)
  (let ((row-list ()))
    (loop for c across unexpanded-row
          do (if (digit-char-p c)
                 (dotimes (i (digit-char-p c)) (push nil row-list))
                 (push c row-list)))
    row-list))                ;This reverses the list because push adds pieces in the wrong order



;;;takes the first parts of the fen string that have been filtered out by fen-to-board-array and returns the board
(defun get-board2d-from-fen-substring (fen-substring)
  (let ((board2d (make-array '(8 8) :initial-element nil)))
    (let ((rows (split-sequence:SPLIT-SEQUENCE #\/ fen-substring)))
      (dolist (row '(8 7 6 5 4 3 2 1))
        (dolist (col '(8 7 6 5 4 3 2 1))
          (setf (aref board2d (- row 1) (- 8 col))
                (nth (- col 1) (expand-fen-row (nth (- row 1) rows)))))))
    board2d))


(defun get-board2d-from-fen (fen)
  (get-board2d-from-fen-substring (car (fen-to-pieces fen))))

(defun square-num-from-fen-square (str)
  (let ((col-index (char str 0)) (row-index (char str 1))
        (col-mapping '(#\a #\b #\c #\d #\e #\f #\g #\h)) (row-mapping '(#\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1)))
    (+ (* 8 (position row-index row-mapping)) (position col-index col-mapping))))

;;;ignore 50 move rule for now
(defun get-game-state-from-fen (fen)
  (let ((parts-of-fen (fen-to-pieces fen)))
    (list :gameboard (board2d-to-gameboard (get-board2d-from-fen fen))
          :to-move (if (equal (nth 1 parts-of-fen) "w") 0 1)
          :castling (nth 2 parts-of-fen)
          :en-passant (if (equal (nth 3 parts-of-fen) "-") -1 (square-num-from-fen-square (nth 3 parts-of-fen))))))


(defun get-gshash-from-gs (game-state)
  (list :gameboard (getf game-state :gameboard)
        :to-move (getf game-state :to-move)
        :castling (castling-perms-to-index (getf game-state :castling))
        :en-passant (getf game-state :en-passant)
        :hash-value (get-zobrist-hash game-state)))

(defun get-gshash-from-fen (fen)
  (get-gshash-from-gs (get-game-state-from-fen fen)))

(defmacro reset-position (game-state)
  `(setf ,game-state (get-game-state-from-fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")))


(defun square-to-notation (square)
  (concatenate 'string
               (nth (get-col-from-square square) '("a" "b" "c" "d" "e" "f" "g" "h"))
               (nth (get-row-from-square square) '("8" "7" "6" "5" "4" "3" "2" "1"))))


;;;This function is useless
;; (defun u64-index-to-rowcol (index)
;;   (list (floor index 8) (logand index 7)))



;;;Gets the location of the occupied squares in gameboard[x], for x in pieces
(defun get-pieces (gameboard pieces)
  (let ((pieces-mask 0))
    (dolist (piece pieces)
      (setf pieces-mask (logior pieces-mask (aref gameboard piece))))
    pieces-mask))

;;;Returns logical or of all of the pieces of the player to move
(defun get-my-pieces (game-state)
  (let ((gameboard (getf game-state :gameboard)))
    (if (eql (getf game-state :to-move) 0)
        (get-pieces gameboard '(0 1 2 3 4 5))
        (get-pieces gameboard '(6 7 8 9 10 11)))))

;;;Return logical or of all the pieces of the player not to move
(defun get-opp-pieces (game-state)
  (let ((gameboard (getf game-state :gameboard)))
    (if (eql (getf game-state :to-move) 0)
        (get-pieces gameboard '(6 7 8 9 10 11))
        (get-pieces gameboard '(0 1 2 3 4 5)))))

(defun get-pieces-wb (game-state color)
  (if (eql color 0)
      (get-pieces (getf game-state :gameboard) '(0 1 2 3 4 5))
      (get-pieces (getf game-state :gameboard) '(6 7 8 9 10 11))))


(defun get-all-pieces (gameboard)
  (get-pieces gameboard '(0 1 2 3 4 5 6 7 8 9 10 11)))


(defun is-occupied-bitboard (bitboard square)
  (not (eql (logand (ash 1 square) bitboard) 0)))

(defun get-locations-piece (gameboard piecename)
  (aref gameboard (piece-to-index piecename)))

;;;This checks if the piece on the square is actually of type piecename.
(defun check-piece-square (gameboard square piecename)
  (not (eql (logand (get-locations-piece gameboard piecename)
                    (ash 1 square))
            0)))


(defun find-piece-type (gameboard square)
  (dolist (piecename '(#\P #\N #\B #\R #\K #\Q #\p #\n #\b #\r #\k #\q))
    (if (check-piece-square gameboard square piecename)
        (return-from find-piece-type (piece-to-index piecename))))
  -1)
