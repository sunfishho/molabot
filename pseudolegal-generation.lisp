

;;;The string is "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
;;;Piece ordering: "PNBRKQpnbrkq"


;;;https://github.com/official-stockfish/Stockfish/blob/master/src/bitboard.h for sliding pieces
;;;https://github.com/official-stockfish/Stockfish/blob/master/src/bitboard.cpp for knight, king movements



(defvar *knight-movements* (make-array '(8) :initial-contents '(-17 -15 -10 -6 6 10 15 17)))
(defvar *king-movements* (make-array '(8) :initial-contents '(-9 -8 -7 -1 1 7 8 9)))

(defvar *rank-8* (coerce 255 '(unsigned-byte 64)))
(defvar *rank-7* (ash *rank-8* 8))
(defvar *rank-6* (ash *rank-8* 16))
(defvar *rank-5* (ash *rank-8* 24))
(defvar *rank-4* (ash *rank-8* 32))
(defvar *rank-3* (ash *rank-8* 40))
(defvar *rank-2* (ash *rank-8* 48))
(defvar *rank-1* (ash *rank-8* 56))

(defvar *file-1* (coerce 72340172838076673 '(unsigned-byte 64)))
(defvar *file-2* (ash *file-1* 1))
(defvar *file-3* (ash *file-1* 2))
(defvar *file-4* (ash *file-1* 3))
(defvar *file-5* (ash *file-1* 4))
(defvar *file-6* (ash *file-1* 5))
(defvar *file-7* (ash *file-1* 6))
(defvar *file-8* (ash *file-1* 7))

(defmacro create-bitboard-array (dim)
  `(make-array ,dim :element-type '(unsigned-byte 64) :initial-element 0))


(defvar *rook-premasks* (create-bitboard-array '(64)))
(defvar *bishop-premasks* (create-bitboard-array '(64)))

(defvar *rook-magic-lookup-table* (create-bitboard-array '(64 4096)))
(defvar *bishop-magic-lookup-table* (create-bitboard-array '(64 512)))

(defvar *rook-magic* (create-bitboard-array '(64)))
(defvar *bishop-magic* (create-bitboard-array '(64)))

(defvar *knight-pseudolegal-table* (create-bitboard-array '(64)))
(defvar *king-pseudolegal-table* (create-bitboard-array '(64)))



(defun get-rank (row)
  (cond ((eql row 0) *rank-8*)
        ((eql row 1) *rank-7*)
        ((eql row 2) *rank-6*)
        ((eql row 3) *rank-5*)
        ((eql row 4) *rank-4*)
        ((eql row 5) *rank-3*)
        ((eql row 6) *rank-2*)
        ((eql row 7) *rank-1*)
        (t (error "ROW OUT OF BOUNDS"))))

(defun get-file (col)
  (cond ((eql col 0) *file-1*)
        ((eql col 1) *file-2*)
        ((eql col 2) *file-3*)
        ((eql col 3) *file-4*)
        ((eql col 4) *file-5*)
        ((eql col 5) *file-6*)
        ((eql col 6) *file-7*)
        ((eql col 7) *file-8*)
        (t (error "ROW OUT OF BOUNDS"))))

(defun get-bitboard-square (row col)
  (logand (get-rank row) (get-file col)))

(defun check-boundary (square)
  (<= 0 square 63))

(defun king-distance (square1 square2)
  (max (abs (- (ash square1 -3) (ash square2 -3)))
       (abs (- (logand square1 7) (logand square2 7)))))

;;;This should set the knight pseudolegal table.
(defun gen-pseudolegal-knight-table ()
  (dotimes (square 64)
    (dotimes (index 8)
      (when (and (check-boundary (+ square (aref *knight-movements* index))) (<= (king-distance square (+ square (aref *knight-movements* index))) 2))
        (setf (aref *knight-pseudolegal-table* square)
              (logior (aref *knight-pseudolegal-table* square) (ash 1 (+ square (aref *knight-movements* index))))))))
  "Knight Table Set")

;;;This should set the king pseudolegal table.
(defun gen-pseudolegal-king-table ()
  (dotimes (square 64)
    (dotimes (index 8)
      (when (and (check-boundary (+ square (aref *king-movements* index))) (<= (king-distance square (+ square (aref *king-movements* index))) 1))
        (setf (aref *king-pseudolegal-table* square)
              (logior (aref *king-pseudolegal-table* square) (ash 1 (+ square (aref *king-movements* index))))))))
  "King Table Set")

;;;Constructs the premask of a rook on a square from 0 to 63 inclusive
(defun construct-rook-premask-square (square)
  (let ((premask 0)
        (row (get-row-from-square square))
        (col (get-col-from-square square)))
    
    (setf premask (logxor (get-rank row)
                          (get-file col)))
    (cond ((and (or (eql row 0) (eql row 7))
                (or (eql col 0) (eql col 7)))
           (setf premask (logxor premask
                                 (get-bitboard-square row 0)
                                 (get-bitboard-square row 7)
                                 (get-bitboard-square 0 col)
                                 (get-bitboard-square 7 col)))
           premask)
          ((or (eql row 0) (eql row 7))
           (setf premask (logxor premask
                                 (get-bitboard-square row 0)
                                 (get-bitboard-square row 7)
                                 (get-bitboard-square (- 7 row) col)))
           premask)
          ((or (eql col 0) (eql col 7))
           (setf premask (logxor premask
                                 (get-bitboard-square 0 col)
                                 (get-bitboard-square 7 col)
                                 (get-bitboard-square row (- 7 col))))
           premask)
          ((not (or (eql row 0) (eql row 7) (eql col 0) (eql col 7)))
           (setf premask (logxor premask
                                 (get-bitboard-square row 0)
                                 (get-bitboard-square row 7)
                                 (get-bitboard-square 0 col)
                                 (get-bitboard-square 7 col)))
           premask))))

;;;Constructs the premask of a bishop on a square from 0 to 63 inclusive
(defun construct-bishop-premask-square (square)
  (let ((pre-mask 0))
    (dotimes (cur-square 64)
      (when (and (eql (abs (- (get-row-from-square square) (get-row-from-square cur-square)))
                      (abs (- (get-col-from-square square) (get-col-from-square cur-square))))
                 (not (eql square cur-square))
                 (<= 1 (get-row-from-square cur-square) 6)
                 (<= 1 (get-col-from-square cur-square) 6))
        (setf pre-mask (logior pre-mask (ash 1 cur-square)))))
    pre-mask))




;;;Populates the premasks and postmasks arrays
;;;This only needs to be called at the start of the program
(defun gen-premasks ()
  (dotimes (square 64)
    (setf (aref *rook-premasks* square) (construct-rook-premask-square square))
    (setf (aref *bishop-premasks* square) (construct-bishop-premask-square square))))

;;;This finds, given a square with a rook or a bishop, the empty squares that are attacked by the piece on the square
(defun find-squares-attacked (square dxdy bboard)
  (let ((squares-attacked 0))
    (dolist (cur-dxdy dxdy)
      (do ((cur-square square (+ cur-square cur-dxdy)))
          ((or (not (check-boundary (+ cur-square cur-dxdy)))
               (is-occupied-bitboard bboard cur-square)
               (not (> 2 (- (logand cur-square 7) (logand (+ cur-square cur-dxdy) 7)) -2))))
        (setf squares-attacked (logior squares-attacked (ash 1 (+ cur-square cur-dxdy))))))
    squares-attacked))

(defun get-legal-moves-rook-index (bboard square)
  (ash (* (aref *rook-magic* square) (logand (aref *rook-premasks* square) bboard)) -52))

(defun get-legal-moves-bishop-index (bboard square)
  (ash (* (aref *bishop-magic* square) (logand (aref *bishop-premasks* square) bboard)) -55))


;;;Idea taken from sparse_rand in misc.h of the Stockfish github
(defun sparse_rand ()
  (logand (random 18446744073709551615)
          (random 18446744073709551615)
          (random 18446744073709551615)))

;;;This generates the lookup table and the magic number for a given square
;;;premasks should be *rook-premasks* or *bishop-premasks*
;;;lookup-table should be *rook-magic-lookup-table* or *bishop-magic-lookup-table*
;;;dxdy should be '(-8 -1 1 8) or '(-9 -7 7 9) respectively
;;;shift should be -52 or -55 respectively
(defun gen-lookup-table (square premasks lookup-table dxdy shift)
  ;;occupancy contains a subset of a premask, while reference contains the squares attacked by the piece given that subset
  ;;We begin by iterating over all possible subsets of the mask premask
  ;;This code is heavily based on init_magics in bboard.cpp of the Stockfish code.
  (let ((occupancy (create-bitboard-array '(4096)))
        (reference (create-bitboard-array '(4096)))
        (attempt-counter (make-array '(4096) :initial-element 0))
        (premask (aref premasks square))
        (first-step t)
        (num-subsets 0)
        (index 0))
    (do ((blockers 0 (logand premask (- blockers premask))))
        ((and (eql blockers 0) (not first-step)))
      (setf first-step nil
            (aref occupancy num-subsets) blockers
            (aref reference num-subsets) (find-squares-attacked square dxdy blockers)
            num-subsets (+ num-subsets 1)))
    (do ((i 0 (+ i 1)))
        ((>= i 10000000))
      ;;turns out lisp puts 2^64-1 as a bignum, so we'll use 2^62-1 instead
      (let ((magic-cand (sparse_rand))
            (muggle nil))
        (do ((j 0 (+ j 1)))
            ((or muggle (>= j num-subsets)))
          ;;;apply magic formula
          (setf index (ash (logand (* (aref occupancy j) magic-cand) 18446744073709551615) shift))
          ;;first condition: either we set the value in a previous attempt, or we haven't set this value at all
          (cond ((or (< (aref attempt-counter index) i)
                     (eql (aref lookup-table square index) 0))
                 (setf (aref lookup-table square index)
                       (aref reference j)
                       (aref attempt-counter index) i))
                ((eql (aref lookup-table square index)
                      (aref reference j))
                 nil)
                (t (setf muggle t))))
        (when (not muggle)
          (return-from gen-lookup-table magic-cand))))
    (error "Failed to find magic number")))

;;;This generates all the rook magic numbers and places them in *rook-magic*
(defun gen-all-rook-magics ()
  (dotimes (square 64)
    (setf (aref *rook-magic* square)
          (gen-lookup-table square
                            *rook-premasks*
                            *rook-magic-lookup-table*
                            '(-8 -1 1 8)
                            -52))))
;;;This generates all the bishop magic numbers and places them in *bishop-magic*
(defun gen-all-bishop-magics ()
  (dotimes (square 64)
    (setf (aref *bishop-magic* square)
          (gen-lookup-table square
                            *bishop-premasks*
                            *bishop-magic-lookup-table*
                            '(-9 -7 7 9)
                            -55))))

;;;This returns a bitboard with all the squares attacked by a rook on this square,
;;;given the bitboard of piece locations. This also doesn't care about what color
;;;any of the pieces are (a piece being protected is therefore counted as "attacked."
(defun get-squares-attacked-rook (bboard square)
  (aref *rook-magic-lookup-table*
        square
        (ash (logand (* (logand bboard (aref *rook-premasks* square))
                        (aref *rook-magic* square))
                     18446744073709551615)
             -52)))


;;;This does the same thing as get-squares-attacked-rook, but for a bishop.
(defun get-squares-attacked-bishop (bboard square)
  (aref *bishop-magic-lookup-table*
        square
        (ash (logand (* (logand bboard (aref *bishop-premasks* square))
                        (aref *bishop-magic* square))
                     18446744073709551615)
             -55)))

;;;This does the same thing as get-squares-attacked-rook, but for a queen.
(defun get-squares-attacked-queen (bboard square)
  (logior (get-squares-attacked-rook bboard square)
          (get-squares-attacked-bishop bboard square)))

(defun boundary-check-pawn-captures (square dxdy)
  (if (and (check-boundary (+ square dxdy))
           (<= (king-distance square (+ square dxdy)) 2))
      (ash 1 (+ square dxdy))
      0))


;;;Get squares attacked by a pawn on this square
(defun get-squares-attacked-pawn (square color)
  (let ((attack-bboard 0))
    (cond ((eql color 0)
           (let ((dxdy '(-7 -9)))
             (dolist (cur-dxdy dxdy)
               (setf attack-bboard (logior attack-bboard
                                           (boundary-check-pawn-captures square cur-dxdy)))))
           (return-from get-squares-attacked-pawn attack-bboard))
          ((eql color 1)
           (let ((dxdy '(7 9)))
             (dolist (cur-dxdy dxdy)
               (setf attack-bboard (logior attack-bboard
                                           (boundary-check-pawn-captures square cur-dxdy)))))
           (return-from get-squares-attacked-pawn attack-bboard))
          (t (error "WRONG COLORED PAWN!")))))

(defun squares-attacked-given-piece (bboard square piecetype)
  (cond ((or (eql piecetype 0) (eql piecetype 6)) (get-squares-attacked-pawn square (if (eql piecetype 0) 0 1)))
        ((or (eql piecetype 1) (eql piecetype 7)) (aref *knight-pseudolegal-table* square))
        ((or (eql piecetype 2) (eql piecetype 8)) (get-squares-attacked-bishop bboard square))
        ((or (eql piecetype 3) (eql piecetype 9)) (get-squares-attacked-rook bboard square))
        ((or (eql piecetype 4) (eql piecetype 10)) (aref *king-pseudolegal-table* square))
        ((or (eql piecetype 5) (eql piecetype 11)) (get-squares-attacked-queen bboard square))
        (t (error "NOT A VALID PIECETYPE"))))







