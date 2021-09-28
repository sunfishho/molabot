

;;;Piece ordering: "PNBRKQpnbrkq"

;;;The idea here is to remove the king of the color we're trying to generate legal moves for, and then
;;;find the squares being attacked by the opponent by looping through each square and checking
;;;*pseudolegal-moves*.

;;;current testpos: 4q3/8/8/2b5/1r4p1/4K3/3P4/R7 w - - 0 1

;;;Need to find all pseudolegal king moves, and not include squares attacked by black

(defvar *white-piecelist* '(0 1 2 3 4 5))
(defvar *black-piecelist* '(6 7 8 9 10 11))
(defvar *exclude-white-king* '(0 1 2 3 5 6 7 8 9 10 11))
(defvar *exclude-black-king* '(0 1 2 3 4 5 6 7 8 9 11))
(defvar *obstructed-rook* (create-bitboard-array '(64 64)))
(defvar *obstructed-bishop* (create-bitboard-array '(64 64)))




(defun can-pawn-capture (game-state square color)
  (let ((en-passant-capture-square (getf game-state :en-passant))
        (opp-pieces (get-pieces-wb game-state (- 1 color))))
    (logand (get-squares-attacked-pawn square color)
            (logior opp-pieces (ash 1 en-passant-capture-square)))))


(defun can-pawn-one-square (bboard square color)
  (let ((dx (if (eql color 0) -8 8)))
    (if (eql (logand (ash 1 (+ square dx)) bboard) 0)
        (ash 1 (+ square dx))
        0)))

(defun can-pawn-two-squares (bboard square color)
  (let ((dx (if (eql color 0) -8 8))
        (second-rank (if (eql color 0) 6 1)))
    (if (and (eql (get-row-from-square square) second-rank)
               (eql (logand (ash 1 (+ square dx)) bboard) 0)
               (eql (logand (ash 1 (+ square (* 2 dx))) bboard) 0))
        (ash 1 (+ square (* 2 dx)))
        0)))


;;;This is a pseudolegal generator of all the places a pawn can end up. This does not consider pins, and does not have anything to do with promotions.
;;;Color should be the same as the color of the pawn on square.
(defun potential-pawn-destinations (game-state square color)
  (let ((bboard (get-all-pieces (getf game-state :gameboard))))
    (logior (can-pawn-capture game-state square color)
            (can-pawn-one-square bboard square color)
            (can-pawn-two-squares bboard square color))))




;;;Given the color of the king we want to look at, find all legal moves. (Castling not included.)
;;;This could alternatively be implemented by checking which squares are neither attacked nor xrayed with blocker = king
(defun find-legal-king-moves (game-state color)
  (let ((no-king-board 0)
        (squares-attacked 0)
        (gameboard (getf game-state :gameboard))
        (piecelist (if (eql color 0) *black-piecelist* *white-piecelist*))
        (exclude-king (if (eql color 0) *exclude-white-king* *exclude-black-king*)))
    (dolist (piecetype exclude-king)
      (or-this no-king-board (aref gameboard piecetype)))
    (dolist (piecetype piecelist)
      (dolist (loc (u64-to-list-of-squares (aref gameboard piecetype)))
        (or-this squares-attacked (squares-attacked-given-piece no-king-board loc piecetype))))
    (logand (squares-attacked-given-piece no-king-board (log2 (aref gameboard (if (eql color 0) 4 10))) 4)
            (lognot squares-attacked)
            (lognot (get-pieces-wb game-state color)))))

;;;Finds the attackers of the same color of a square (i.e. if color = 0, find all white pieces attacking square).
(defun find-attackers (gameboard color square)
  (let ((attackers 0)
        (piece-list (if (eql color 0) '(0 1 2 3 4 5) '(6 7 8 9 10 11))))
    (dolist (piece-index piece-list)
      (or-this attackers (logand (squares-attacked-given-piece (get-all-pieces gameboard)
                                                               square
                                                               (if (< piece-index 6) (+ piece-index 6) (- piece-index 6)))
                                 (aref gameboard piece-index))))
    attackers))

;;;Note that this assumes square is currently empty (i.e. self captures are possible for non-pawns), and does not check if the occupiers are pinned.
(defun find-potential-occupiers (game-state color square)
  (let* ((gameboard (getf game-state :gameboard))
         (occupiers 0) (dx (if (eql color 0) -8 8))
         (piece-list (if (eql color 0) '(1 2 3 4 5) '(7 8 9 10 11)))
         (bboard (get-all-pieces gameboard)))
    (when (not (eql (logand (get-pieces-wb game-state color) (ash 1 square)) 0))
      (return-from find-potential-occupiers 0))
    (dolist (piece-index piece-list)
      (or-this occupiers (logand (squares-attacked-given-piece (get-all-pieces gameboard)
                                                               square
                                                               (if (< piece-index 6) (+ piece-index 6) (- piece-index 6)))
                                 (aref gameboard piece-index))))
    ;;This checks 1-square pawn moves by checking that there's a pawn 1 square behind and that it can actually move
    (when (and (check-piece-square gameboard (- square dx) (if (eql color 0) #\P #\p))
               (not (eql (can-pawn-one-square bboard (- square dx) color) 0)))
      (or-this occupiers (ash 1 (- square dx))))
    ;;Can move a pawn 2 squares if we are on rank 4 if white/5 if black, if the square behind is empty, and if there is a pawn 2 squares behind
    (when (and (check-piece-square gameboard (- square (* 2 dx)) (if (eql color 0) #\P #\p))
               (not (eql (can-pawn-two-squares bboard (- square (* 2 dx)) color) 0)))
      (or-this occupiers (ash 1 (- square (* 2 dx)))))
    ;;A pawn can capture onto this square from the left
    (when (and (check-piece-square gameboard (- square dx 1) (if (eql color 0) #\P #\p))
               (not (eql (logand (can-pawn-capture game-state (- square dx 1) color)
                                 (ash 1 square))
                         0)))
      (or-this occupiers (ash 1 (- square dx 1))))
    ;;A pawn can capture onto this square from the right
    (when (and (check-piece-square gameboard (- square dx -1) (if (eql color 0) #\P #\p))
               (not (eql (logand (can-pawn-capture game-state (- square dx -1) color)
                                 (ash 1 square))
                         0)))
      (or-this occupiers (ash 1 (- square dx -1))))
    occupiers))

;;;This should find a bitboard containing all the locations of up to 2 checking pieces
;;;bitboard is the current board, square should be the location of the king, and color should be the color of the king
;;;This doesn't work for pawns
(defun find-checkers (gameboard color)
  (find-attackers gameboard (- 1 color) (log2 (aref gameboard (if (eql color 0) 4 10)))))

;;;Returns the number of checks that this side is in (either 0, 1 or 2).
;;;If this function returns 2, the only legal moves will be king moves (use find-legal-king-moves)
(defun number-checks (gameboard color)
  (let ((checkers (find-checkers gameboard color)))
    (cond ((eql checkers 0) 0)
          ((eql (logxor checkers (lsb checkers)) 0) 1)
          (t 2))))

;;;Finds the set of squares that are blocked off by blockers from a rook/bishop on square, given bitboard is the position
;;;and piecetype is either #\B, #\R, or #\Q
;;;This code is largely from chessprogramming.org
;;;https://www.chessprogramming.org/X-ray_Attacks_(Bitboards)#ModifyingOccupancy
(defun xray-attacks (bboard potential-blockers square piecetype)
  (let ((attacks (squares-attacked-given-piece bboard square piecetype)))
    (logxor attacks
            (squares-attacked-given-piece (logxor bboard
                                                  (logand potential-blockers attacks))
                                          square
                                          piecetype))))
;;;Implementation based on this:
;;;https://www.chessprogramming.org/Checks_and_Pinned_Pieces_(Bitboards)
(defun find-pins-rook (gameboard color)
  (let ((bitboard (get-pieces gameboard '(0 1 2 3 4 5 6 7 8 9 10 11)))
        (my-pieces (get-pieces gameboard (if (eql color 0) '(0 1 2 3 4 5) '(6 7 8 9 10 11))))
        (king-square  (log2 (aref gameboard (piece-to-index (if (eql color 0) #\K #\k)))))
        (opp-RQ (if (eql color 0) (logior (get-locations-piece gameboard #\r) (get-locations-piece gameboard #\q))
                    (logior (get-locations-piece gameboard #\R) (get-locations-piece gameboard #\Q))))
        (pinned 0))
    ;;In theory, the color of the rook should not matter because xray-attacks only cares about what type of piece it is
    (do ((pinner (logand (xray-attacks bitboard my-pieces king-square (piece-to-index #\R))
                                    opp-RQ)))
        ((eql pinner 0))
      ;;Let sq be a pinner
      (let ((sq (log2 (lsb pinner))))
        (setf pinned (logior pinned
                             (logand my-pieces
                                     (aref *obstructed-rook* sq king-square)))))
      ;;Clears lsb
      (setf pinner (logand pinner (- pinner 1))))
    pinned))

(defun find-pins-bishop (gameboard color)
  (let ((bitboard (get-pieces gameboard '(0 1 2 3 4 5 6 7 8 9 10 11)))
        (my-pieces (get-pieces gameboard (if (eql color 0) '(0 1 2 3 4 5) '(6 7 8 9 10 11))))
        (king-square  (log2 (aref gameboard (piece-to-index (if (eql color 0) #\K #\k)))))
        (opp-BQ (if (eql color 0) (logior (get-locations-piece gameboard #\b) (get-locations-piece gameboard #\q))
                    (logior (get-locations-piece gameboard #\B) (get-locations-piece gameboard #\Q))))
        (pinned 0))
    ;;In theory, the color of the rook should not matter because xray-attacks only cares about what type of piece it is
    (do ((pinner (logand (xray-attacks bitboard my-pieces king-square (piece-to-index #\B))
                         opp-BQ)))
        ((eql pinner 0))
      ;;Let sq be a pinner
      (let ((sq (log2 (lsb pinner))))
        (setf pinned (logior pinned
                             (logand my-pieces
                                     (aref *obstructed-bishop* sq king-square)))))
      ;;Clears lsb
      (setf pinner (logand pinner (- pinner 1))))
    pinned))

(defun find-pins (gameboard color)
  (logior (find-pins-bishop gameboard color) (find-pins-rook gameboard color)))

;;;sq1 and sq2 should either be on the same row or column
(defun gen-obstructed-rook ()
  (dotimes (sq1 64)
    (dotimes (sq2 64)
      (cond ((>= sq1 sq2)
             ())
        ((eql (get-row-from-square sq1) (get-row-from-square sq2))
         (do ((sq (+ sq1 1) (+ sq 1)))
             ((eql sq sq2))
           (setf (aref *obstructed-rook* sq1 sq2) (logior (aref *obstructed-rook* sq1 sq2)
                                                          (ash 1 sq))
                 (aref *obstructed-rook* sq2 sq1) (logior (aref *obstructed-rook* sq2 sq1)
                                                          (ash 1 sq)))))
        ((eql (get-col-from-square sq1) (get-col-from-square sq2))
         (do ((sq (+ sq1 8) (+ sq 8)))
             ((eql sq sq2))
           (setf (aref *obstructed-rook* sq1 sq2) (logior (aref *obstructed-rook* sq1 sq2)
                                                          (ash 1 sq))
                 (aref *obstructed-rook* sq2 sq1) (logior (aref *obstructed-rook* sq2 sq1)
                                                          (ash 1 sq))))))))
  "*OBSTRUCTED-ROOK* POPULATED")

(defun on-same-diagonal (sq1 sq2)
  (let ((r1 (get-row-from-square sq1))
        (r2 (get-row-from-square sq2))
        (c1 (get-col-from-square sq1))
        (c2 (get-col-from-square sq2)))
    (eql (abs (- r1 r2)) (abs (- c1 c2)))))

(defun gen-obstructed-bishop ()
  (dotimes (sq1 64)
    (dotimes (sq2 64)
      (cond ((or (>= sq1 sq2) (not (on-same-diagonal sq1 sq2)))
             ())
            ((eql (mod (- sq2 sq1) 9) 0)
             (do ((sq (+ sq1 9) (+ sq 9)))
                 ((eql sq sq2))
               (setf (aref *obstructed-bishop* sq1 sq2) (logior (aref *obstructed-bishop* sq1 sq2)
                                                              (ash 1 sq))
                     (aref *obstructed-bishop* sq2 sq1) (logior (aref *obstructed-bishop* sq2 sq1)
                                                                (ash 1 sq)))))
            ((eql (mod (- sq2 sq1) 7) 0)
             (do ((sq (+ sq1 7) (+ sq 7)))
                 ((eql sq sq2))
               (setf (aref *obstructed-bishop* sq1 sq2) (logior (aref *obstructed-bishop* sq1 sq2)
                                                                (ash 1 sq))
                     (aref *obstructed-bishop* sq2 sq1) (logior (aref *obstructed-bishop* sq2 sq1)
                                                                (ash 1 sq)))))
            (t (error "LOGIC ERROR IN GEN-OBSTRUCTED-BISHOP")))))
  "*OBSTRUCTED-BISHOP* POPULATED")

;;;Based on the Move type in https://github.com/official-stockfish/Stockfish/blob/master/src/types.h
;;;Promotion piece type is 0 if queen, 1 if knight, 2 if rook, 3 if bishop
;;;move-flag is 0 if normal, 1 if promotion, 2 if en-passant, 3 if castle
(defun encode-move (start finish promotion move-flag)
  (+ start (ash finish 6) (ash promotion 12) (ash move-flag 14)))


;;;This translates the encoded form of the move into a property list for readability.
(defun decode-move (move)
  (list :start (logand move 63)
        :finish (logand (ash move -6) 63)
        :promotion (logand (ash move -12) 3)
        :move-flag (logand (ash move -14) 3)))

;;;Takes the
(defun get-king-moves (game-state move-list)
  (let* ((destinations (find-legal-king-moves game-state (getf game-state :to-move)))
         (gameboard (getf game-state :gameboard))
         (color (getf game-state :to-move))
         (king-sq (log2 (get-locations-piece gameboard (if (eql color 0) #\K #\k)))))
    (do ((dest (lsb destinations) (lsb destinations)))
        ((eql destinations 0))
      (setf destinations (logand destinations (- destinations 1)))
      (vector-push-extend (encode-move king-sq (log2 dest) 0 0) move-list))
    move-list))


(defun check-if-en-passant (game-state start finish)
  (let* ((gameboard (getf game-state :gameboard))
         (color (getf game-state :to-move))
         (en-passant-capture-square (getf game-state :en-passant)))
    (and (eql finish en-passant-capture-square)
         (check-piece-square gameboard start (if (eql color 0) #\P #\p)))))


(defun check-if-promotion (game-state start finish)
  (and (if (eql (getf game-state :to-move) 0) (< finish 8) (< 55 finish))
       (check-piece-square (getf game-state :gameboard) start (if (eql (getf game-state :to-move) 0) #\P #\p))))

;;;Checker should be the index of the square that is currently checking the king
;;;Need to encode the promotion, en-passant as so
(defun get-moves-blocking-check (game-state checker pinned-pieces move-list)
  (let* ((gameboard (getf game-state :gameboard))
         (color (getf game-state :to-move))
         (king-sq (log2 (get-locations-piece gameboard (if (eql color 0) #\K #\k))))
         (blockpoints (if (on-same-diagonal checker king-sq) (aref *obstructed-bishop* checker king-sq) (aref *obstructed-rook* checker king-sq))))
    ;;;iterate through blockpoints and find if any of these are attacked by a piece that are not pinned
    (if (eql blockpoints 0)
        (return-from get-moves-blocking-check 0))
    (while (not (eql blockpoints 0))
      (let* ((blockpoint (log2 (lsb blockpoints)))
            (blockers (logand (find-potential-occupiers game-state color blockpoint)
                              (lognot pinned-pieces)
                              (lognot (ash 1 king-sq)))))
        (while (not (eql blockers 0))
          (let ((blocker (log2 (lsb blockers))))
            ;;Check if it's a promotion
            (cond ((check-if-promotion game-state blocker blockpoint)
                   (vector-push-extend (encode-move blocker blockpoint 0 1) move-list)
                   (vector-push-extend (encode-move blocker blockpoint 1 1) move-list)
                   (vector-push-extend (encode-move blocker blockpoint 2 1) move-list)
                   (vector-push-extend (encode-move blocker blockpoint 3 1) move-list))
                  (t (vector-push-extend (encode-move blocker blockpoint 0 0) move-list))))
          (and-this blockers (- blockers 1)))
        (and-this blockpoints (- blockpoints 1))
        ))
    move-list))


(defun get-moves-taking-checker (game-state checker pinned-pieces move-list)
  (let* ((gameboard (getf game-state :gameboard))
         (color (getf game-state :to-move))
         (capturers (logand (find-attackers gameboard color checker)
                            (lognot pinned-pieces)))
         (en-passant-capture-square (getf game-state :en-passant)))
    (while (and (not (eql capturers 0)))
      (let ((capturer (log2 (lsb capturers))))
        (when (not (check-piece-square gameboard capturer (if (eql color 0) #\K #\k)))
          (cond ((check-if-promotion game-state capturer checker)
                 (vector-push-extend (encode-move capturer checker 0 1) move-list)
                 (vector-push-extend (encode-move capturer checker 1 1) move-list)
                 (vector-push-extend (encode-move capturer checker 2 1) move-list)
                 (vector-push-extend (encode-move capturer checker 3 1) move-list))
                (t (vector-push-extend (encode-move capturer checker 0 0) move-list)))))
      (and-this capturers (- capturers 1)))
    ;;To en-passant, the pawn capturing cannot be pinned to the king
    (when (not (eql en-passant-capture-square -1))
      (let ((pawn (if (eql color 0) #\P #\p)))
        (when (and (check-piece-square gameboard (+ checker 1) pawn)
                   (eql (logand (ash 1 (+ checker 1)) pinned-pieces) 0))
          (vector-push-extend (encode-move (+ checker 1) en-passant-capture-square 0 2) move-list))
        (when (and (check-piece-square gameboard (- checker 1) pawn)
                   (eql (logand (ash 1 (- checker 1)) pinned-pieces) 0))
          (vector-push-extend (encode-move (- checker 1) en-passant-capture-square 0 2) move-list))))
    move-list))

;;;Find all moves involving pinned pieces
(defun get-pinned-moves (game-state pinned-pieces move-list)
  (let* ((gameboard (getf game-state :gameboard))
        (bboard (get-all-pieces gameboard))
        (color (getf game-state :to-move)))
    (and-this pinned-pieces
        (lognot (aref gameboard 1))
        (lognot (aref gameboard 4))
        (lognot (aref gameboard 7))
        (lognot (aref gameboard 10)))
    (while (not (eql pinned-pieces 0))
      (let* ((pinned-piece (log2 (lsb pinned-pieces)))
             (king-sq (log2 (aref gameboard (if (eql color 0) 4 10))))
             (pintype (if (or (eql (get-row-from-square king-sq) (get-row-from-square pinned-piece))
                              (eql (get-col-from-square king-sq) (get-col-from-square pinned-piece)))
                          (piece-to-index #\R)
                          (piece-to-index #\B)))
             (pinner-sq (log2 (logand (xray-attacks bboard (ash 1 pinned-piece) king-sq pintype)
                                      (logior (get-locations-piece gameboard (if (eql (- 1 color) 0) #\Q #\q))
                                              (get-locations-piece gameboard (if (eql pintype (piece-to-index #\R))
                                                                                 (if (eql (- 1 color) 0) #\R #\r)
                                                                                 (if (eql (- 1 color) 0) #\B #\b)))))))
             (possible-destinations (logior (aref (if (eql pintype (piece-to-index #\R)) *obstructed-rook* *obstructed-bishop*) king-sq pinner-sq)
                                            (ash 1 pinner-sq))))
        ;;;Either the pinned piece is a pawn or it isn't.
        (and-this possible-destinations (if (or (check-piece-square gameboard pinned-piece #\P)
                                                (check-piece-square gameboard pinned-piece #\p))
                                            (potential-pawn-destinations game-state pinned-piece color)
                                            (squares-attacked-given-piece bboard pinned-piece (find-piece-type gameboard pinned-piece))))
        (while (not (eql possible-destinations 0))
          (let ((destination (log2 (lsb possible-destinations))))
            ;;En-passant has not been dealt with
            (vector-push-extend (encode-move pinned-piece destination 0 (if (check-if-en-passant game-state pinned-piece destination) 2 0)) move-list))
          (and-this possible-destinations (- possible-destinations 1)))
        )
      (and-this pinned-pieces (- pinned-pieces 1)))
    move-list))
;;;Later, find a way to check for the "en-passant" horizontal pin


;;;We assume when calling this function piece is not pinned.
(defun find-destinations (game-state piece piecetype)
  (let ((bboard (get-all-pieces (getf game-state :gameboard)))
        (color (getf game-state :to-move)))
    (cond ((eql piecetype (if (eql color 0) 0 6))
           (potential-pawn-destinations game-state piece color))
          ((eql piecetype (if (eql color 0) 4 10))
           (find-legal-king-moves game-state color))
          (t (logand (squares-attacked-given-piece bboard piece piecetype)
                     (lognot (get-my-pieces game-state)))))))



(defun get-all-moves-no-check (game-state pinned-pieces move-list)
  (let ((gameboard (getf game-state :gameboard))
        (my-pieces (logand (get-my-pieces game-state)
                           (lognot pinned-pieces))))
    (while (not (eql my-pieces 0))
      (let* ((piece (log2 (lsb my-pieces)))
             ;;create a function to, given a piece, come up with all places the piece can move to
             ;;this function should behave very similarly to squares-attacked, except it should treat pawns differently
             (destinations (find-destinations game-state piece (find-piece-type gameboard piece))))
        (while (not (eql destinations 0))
          (let ((destination (log2 (lsb destinations))))
            (cond ((check-if-promotion game-state piece destination)
                   (vector-push-extend (encode-move piece destination 0 1) move-list)
                   (vector-push-extend (encode-move piece destination 1 1) move-list)
                   (vector-push-extend (encode-move piece destination 2 1) move-list)
                   (vector-push-extend (encode-move piece destination 3 1) move-list))
                  (t (vector-push-extend (encode-move piece destination 0 (if (check-if-en-passant game-state piece destination) 2 0)) move-list))))
          (and-this destinations (- destinations 1))))
      (and-this my-pieces (- my-pieces 1)))
    (get-pinned-moves game-state pinned-pieces move-list)
    move-list))


;;;This is the main move generation function

(defun generate-all-legal-moves (game-state)
  (let* ((gameboard (getf game-state :gameboard))
         (color (getf game-state :to-move))
         (num-checks (number-checks gameboard color))
         (legal-moves (make-array 0 :fill-pointer 0 :adjustable t)))
    (cond ((eql num-checks 0)
           (get-all-moves-no-check game-state (find-pins gameboard color) legal-moves)
           legal-moves)
          ((eql num-checks 1)
           (let ((checker (log2 (find-checkers gameboard color)))
                 (pinned-pieces (find-pins gameboard color)))
             ;;;Check if the check is from a non-sliding piece
             (cond ((or (check-piece-square gameboard checker #\N)
                        (check-piece-square gameboard checker #\P)
                        (check-piece-square gameboard checker #\n)
                        (check-piece-square gameboard checker #\p))
                    (get-king-moves game-state legal-moves)
                    (get-moves-taking-checker game-state checker pinned-pieces legal-moves)
                    legal-moves)
                   (t
                    (get-moves-taking-checker game-state checker pinned-pieces legal-moves)
                    (get-moves-blocking-check game-state checker pinned-pieces legal-moves)
                    (get-king-moves game-state legal-moves)
                    legal-moves))))
          ((eql num-checks 2)
           (get-king-moves game-state legal-moves)
           legal-moves)
          (t (error "OH NO THERE'S A TRIPLE CHECK")))))


