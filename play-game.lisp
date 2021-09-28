
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))


(defun case-insensitive-check-strings (s1 s2)
  (equal (string-downcase s1) (string-downcase s2)))



(defun play-move (game-state movestr)
  (when (case-insensitive-check-strings movestr "game over") (return-from play-move nil))
  (let* ((origin (square-num-from-fen-square (subseq movestr 0 3)))
         (destination (square-num-from-fen-square (subseq movestr 2 4)))
         ;;Castling has not been implemented yet
         (move-type (cond ((check-if-en-passant game-state origin destination) 2)
                          ((check-if-promotion game-state origin destination) 1)
                          (t 0)))
         (promotion-piece-type (if (eql move-type 1)
                                   (let ((piece-type (prompt-read "What type of piece will you promote too?")))
                                     (cond ((case-insensitive-check-strings piece-type "queen") 0)
                                           ((case-insensitive-check-strings piece-type "knight") 1)
                                           ((case-insensitive-check-strings piece-type "rook") 2)
                                           ((case-insensitive-check-strings piece-type "bishop") 2)
                                           (t (error "This is not a valid type of promotion piece, please try again."))))
                                   0)))
    (apply-move game-state (encode-move origin destination promotion-piece-type move-type))
    game-state))

(defun takeback-move (game-state move destination-piece-type en-passant-rights castling-rights)
  (undo-move game-state move destination-piece-type en-passant-rights castling-rights))

(defun find-best-move-molabot (game-state depth)
  (multiple-value-bind (eval PV)
      (alpha-beta game-state depth)
    (print eval)
    (print (mapcar #'get-origin-destination PV))
    (car PV)))

(defun play-molabot (gshash playercolor depth)
  (when (eql playercolor (- 1 (getf gshash :to-move)))
    (let ((best-move (find-best-move-molabot gshash depth)))
      (print (get-origin-destination best-move))
      (apply-move gshash best-move)))
  (while (play-move gshash (prompt-read "What move do you want to play?"))
    (print (getf gshash :hash-value))
    (reset-transposition-table)
    (let ((best-move (find-best-move-molabot gshash depth)))
      (print (get-origin-destination best-move))
      (apply-move gshash best-move))
    (print-gameboard gshash)))
