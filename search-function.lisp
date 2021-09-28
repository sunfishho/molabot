

(defvar *position-counter* 0)


(defun reset-position-counter ()
  (setf *position-counter* 0))

(defmacro reset-game-state (game-state pos)
  `(setf ,game-state (get-game-state-from-fen ,pos)))

(defmacro reset-gshash (gshash pos)
  `(setf ,gshash (get-gshash-from-fen ,pos)))

(defun print-gameboard (gshash)
  (print (gameboard-to-board2d (getf gshash :gameboard))))

;;;max-player is 0 if the player to move is currently trying to maximize their evaluation function.
(defun alpha-beta (gshash depth &optional (alpha -1000000000) (beta 1000000000) (depth-searched 0))
  (incf *position-counter*)
  (let ((alpha-orig alpha)
        (beta-orig beta))
    (let ((stored-vals (gethash (getf gshash :hash-value) *transposition-table*)))
      (when (and stored-vals
                 (>= (nth 0 stored-vals) (- depth depth-searched)))
        ;;If the value stored is exact
        (when (eql (nth 3 stored-vals) 0)
          (return-from alpha-beta (values (nth 1 stored-vals) (if (nth 2 stored-vals) (list (nth 2 stored-vals)) nil))))
        ;;If the value stored was a lower bound
        (when (eql (nth 3 stored-vals) 1)
          (setf alpha (max alpha (nth 1 stored-vals))))
        ;;If the value stored was an upper bound
        (when (eql (nth 3 stored-vals) 2)
          (setf beta (min beta (nth 1 stored-vals))))
        (when (>= alpha beta)
          (return-from alpha-beta (values (nth 1 stored-vals) (if (nth 2 stored-vals) (list (nth 2 stored-vals)) nil))))))
    (when (eql depth-searched depth)
      ;;By this point, we would have already added the PV to the table if we had it stored.
      (add-eval-depth (getf gshash :hash-value) (evaluate gshash) nil 0 0)
      (return-from alpha-beta (values (evaluate gshash) nil)))
    ;;From testing, 3 seems to be the best number to use here.
    (let* ((legal-moves (if (> (- depth depth-searched) 3) (generate-sorted-moves gshash) (generate-all-legal-moves gshash)))
           (num-legal-moves (length legal-moves))
           (color (getf gshash :to-move))
           (best-moves ())
           (cur-eval (if (eql color 0) -1000000000 1000000000)))
      (when (eql num-legal-moves 0)
        (if (eql (find-checkers (getf gshash :gameboard) color) 0)
            (return-from alpha-beta (values 0 nil))
            ;;This is to ensure that the bot finds the shortest mate...
            (return-from alpha-beta (values (* (+ -100000 depth-searched) (- 1 (* 2 color))) nil))))
      
      (dotimes (i num-legal-moves)
        (multiple-value-bind (destination-piece-type en-passant-rights castling-rights)
            (apply-move gshash (aref legal-moves i))
          (multiple-value-bind (eval-move PV) (alpha-beta gshash
                                                          depth
                                                          alpha
                                                          beta
                                                          (+ depth-searched 1))
            (when (if (eql color 0) (< cur-eval eval-move) (< eval-move cur-eval))
              (setf cur-eval eval-move
                    best-moves (cons (aref legal-moves i) PV))))
          (undo-move gshash (aref legal-moves i) destination-piece-type en-passant-rights castling-rights))
        (when (eql color 0)
          (setf alpha (max alpha cur-eval)))
        (when (eql color 1)
          (setf beta (min beta cur-eval)))
        (when (>= alpha beta)
          (add-eval-depth (getf gshash :hash-value)
                          cur-eval
                          (aref legal-moves i)
                          (- depth depth-searched)
                          (cond ((<= cur-eval alpha-orig) 2)
                                ((>= cur-eval beta-orig) 1)
                                (t 0)))
          (return-from alpha-beta (values cur-eval best-moves))))
      ;; (add-eval-depth (getf gshash :hash-value) cur-eval best-moves (- depth depth-searched))
      (add-eval-depth (getf gshash :hash-value) cur-eval (nth 0 best-moves) (- depth depth-searched) (cond ((<= cur-eval alpha-orig) 2)
                                                                                                           ((>= cur-eval beta-orig) 1)
                                                                                                           (t 0)))
      (return-from alpha-beta (values cur-eval best-moves)))))


(defun iterative-deepening (gshash depth)
  (dotimes (i (- depth 1))
    (alpha-beta gshash i))
  (alpha-beta gshash depth))



(defun get-origin-destination (move)
  (mapcar #'square-to-notation (list (get-move-origin move) (get-move-destination move))))

(defun print-eval-pv (gshash depth)
  (multiple-value-bind (evaluation PV) (iterative-deepening gshash depth)
    (print evaluation)
    (print PV)
    (mapcar #'get-origin-destination PV)))
