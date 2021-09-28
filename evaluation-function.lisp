




(defun eval-piece-values (gshash)
  
  (let ((piece-values (make-array '(12) :initial-contents '( 100  320  330  500  20000  900
                                                            -100 -320 -330 -500 -20000 -900)))
        (eval 0))
    (dotimes (i 12)
      (incf eval (* (aref piece-values i) (logcount (aref (getf gshash :gameboard) i)))))
    eval))

(defun count-non-pawn-piece-vals (gshash)
  (let ((piece-values (make-array '(12) :initial-contents '(0 320 330 500 0 900 0 320 330 500 20000 900)))
        (eval 0))
    (dotimes (i 12)
      (incf eval (* (aref piece-values i) (logcount (aref (getf gshash :gameboard) i)))))
    eval))


;;;These eval tables are modified from https://www.chessprogramming.org/Simplified_Evaluation_Function
(defvar *wpawn-locs-eval* (make-array '(64) :initial-contents '(0   0   0   0   0   0   0   0
                                                                50  50  50  50  50  50  50  50
                                                                10  10  20  30  30  20  10  10
                                                                5   5   10  25  25  10  5   5
                                                                0   0   5   20  20  0   0   0
                                                                5   -5  -10 0   0   -10 -5  5
                                                                5   10  10  -20 -20 10  10  5
                                                                0   0   0   0   0   0   0   0)))

(defvar *bpawn-locs-eval* (make-array '(64) :initial-contents '(0   0   0   0   0   0   0   0
                                                                -5  -10 -10 20  20  -10 -10 -5
                                                                -5  5   10  0   0   10  5   -5
                                                                0   0   -5  -20 -20 0   0   0
                                                                -5  -5  -10 -25 -25 -10 -5  -5
                                                                -10 -10 -20 -30 -30 -20 -10 -10
                                                                -50 -50 -50 -50 -50 -50 -50 -50
                                                                0   0   0   0   0   0   0   0)))

(defvar *wknight-locs-eval* (make-array '(64) :initial-contents '(-50 -40 -30 -30 -30 -30 -40 -50
                                                                  -40 -20 0   0   0   0   -20 -40
                                                                  -30 0   20  30  30  20  0   -30
                                                                  -30 5   15  20  20  20  10  -30
                                                                  -30 5   15  20  20  15  5   -30
                                                                  -30 5   10  15  15  10  5   -30
                                                                  -40 -20 0   5   5   0   -20 -40
                                                                  -50 -40 -30 -30 -30 -30 -40 -50)))

(defvar *bknight-locs-eval* (make-array '(64) :initial-contents '(50  40  30  30  30  30  40  50
                                                                  40  20  0   -5  -5  0   20  40
                                                                  30 -5  -10 -15 -15 -10  -5  30
                                                                  30 -5  -15 -20 -20 -20  -5  30
                                                                  30  -5 -15 -20 -20 -15 -10  30
                                                                  30  0  -20 -30 -30 -20  0   30
                                                                  40  20 0   0   0   0    20  40
                                                                  50  40 30  30  30  30   40  50)))

(defvar *wking-midgame-locs-eval* (make-array '(64) :initial-contents '(-40 -40 -40 -40 -40 -40 -40 -40
                                                                        -40 -40 -40 -40 -40 -40 -40 -40
                                                                        -30 -30 -40 -40 -40 -40 -30 -30
                                                                        -30 -30 -30 -30 -30 -30 -30 -30
                                                                        -30 -30 -30 -30 -30 -30 -30 -30
                                                                        -20 -20 -20 -20 -20 -20 -20 -20
                                                                        10  10  0   0   0   0   10  10
                                                                        20  30  10  0   10  0   30  20)))

(defvar *bking-midgame-locs-eval* (make-array '(64) :initial-contents '(20  30  10  0   10   0   30  20
                                                                        10  10  0   0   0    0   10  10
                                                                        -20 -20 -20 -20 -20 -20 -20 -20
                                                                        -30 -30 -30 -30 -30 -30 -30 -30
                                                                        -30 -30 -30 -30 -30 -30 -30 -30
                                                                        -30 -30 -40 -40 -40 -40 -30 -30
                                                                        -40 -40 -40 -40 -40 -40 -40 -40
                                                                        -40 -40 -40 -40 -40 -40 -40 -40)))

(defvar *wking-endgame-locs-eval* (make-array '(64) :initial-contents '(-50 -40 -30 -20 -20 -30 -40 -50
                                                                        -30 -20 -10 0   0   -10 -20 -30
                                                                        -30 -10 20  30  30  20  -10 -30
                                                                        -30 -10 30  40  40  30  -10 -30
                                                                        -30 -10 30  40  40  30  -10 -30
                                                                        -30 -10 20  30  30  20  -10 -30
                                                                        -30 -30 0   0   0   0   -30 -30
                                                                        -50 -30 -30 -30 -30 -30 -30 -50)))

(defvar *bking-endgame-locs-eval* (make-array '(64) :initial-contents '(50  30  30  30  30  30  30  50
                                                                        30  30  0   0   0   0   30  30
                                                                        30  10  -20 -30 -30 -20 10  30
                                                                        30  10  -30 -40 -40 -30 10  30
                                                                        30  10  -30 -40 -40 -30 10  30
                                                                        30  10  -20 -30 -30 -20 10  30
                                                                        30  10  -20 -30 -30 -20 10  30
                                                                        50  40  30  20  20  30  40  50)))

(defvar *wbishop-locs-eval* (make-array '(64) :initial-contents '(-20 -10 -10 -10 -10 -10 -10 -20
                                                                  -10 0   0   0   0   0   0   0
                                                                  -10 0   5   10  10  5   0   -10
                                                                  -10 5   5   10  10  5   5   -10
                                                                  -10 0   10  10  10  10  5   -10
                                                                  -10 10  10  10  10  10  10  -10
                                                                  -10 5   0   0   0   0   5   -10
                                                                  -20 -10 -10 -10 -10 -10 -10 -20)))

(defvar *bbishop-locs-eval* (make-array '(64) :initial-contents '(20  10  10  10  10  10  10  20
                                                                  10  -5  0   0   0   0   -5  10
                                                                  10  -10 -10 -10 -10 -10 -10 10
                                                                  10  0   -10 -10 -10 -10 0   10
                                                                  10  -5  -5  -10 -10 -5  -5  10
                                                                  10  0   -5  -10 -10 -5  0   10
                                                                  10  0   0   0   0   0   0   0
                                                                  20  10  10  10  10  10  10  20)))


(defvar *wrook-locs-eval* (make-array '(64) :initial-contents '(0   0   0   0   0   0   0   0
                                                                5   10  10  10  10  10  10  5
                                                                -5  0   0   0   0   0   0   -5
                                                                -5  0   0   0   0   0   0   -5
                                                                -5  0   0   0   0   0   0   -5
                                                                -5  0   0   0   0   0   0   -5
                                                                -5  0   0   0   0   0   0   -5
                                                                0   0   0   5   5   0   0   0)))

(defvar *brook-locs-eval* (make-array '(64) :initial-contents '(0   0   0   -5  -5  0   0   0
                                                                5   0   0   0   0   0   0   5
                                                                5   0   0   0   0   0   0   5
                                                                5   0   0   0   0   0   0   5
                                                                5   0   0   0   0   0   0   5
                                                                5   0   0   0   0   0   0   5
                                                                -5  -10 -10 -10 -10 -10 -10 -5
                                                                0   0   0   0   0   0   0   0)))


(defun eval-piece-locs (gshash piecename arr)
  (let ((eval 0)
        (bboard (get-locations-piece (getf gshash :gameboard) piecename)))
    (while (not (eql bboard 0))
      (let ((piece (log2 (lsb bboard))))
        (incf eval (aref arr piece)))
      (and-this bboard (- bboard 1)))
    eval))



(defun is-endgame (gshash)
  (< (count-non-pawn-piece-vals gshash) 2500))


(defun eval-locs (gshash)
  (+ (eval-piece-locs gshash #\P *wpawn-locs-eval*)
     (eval-piece-locs gshash #\p *bpawn-locs-eval*)
     (eval-piece-locs gshash #\N *wknight-locs-eval*)
     (eval-piece-locs gshash #\n *bknight-locs-eval*)
     (eval-piece-locs gshash #\K (if (is-endgame gshash) *wking-endgame-locs-eval* *wking-midgame-locs-eval*))
     (eval-piece-locs gshash #\k (if (is-endgame gshash) *bking-endgame-locs-eval* *bking-midgame-locs-eval*))
     (eval-piece-locs gshash #\B *wbishop-locs-eval*)
     (eval-piece-locs gshash #\b *bbishop-locs-eval*)
     (eval-piece-locs gshash #\R *wrook-locs-eval*)
     (eval-piece-locs gshash #\r *brook-locs-eval*)))


(defun evaluate (game-state)
  (+ (eval-piece-values game-state)
     (eval-locs game-state)))
