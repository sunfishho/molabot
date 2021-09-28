;;;Run this program only after every other file has been compiled

(defun init-program ()
  (gen-pseudolegal-king-table)
  (gen-pseudolegal-knight-table)
  (gen-premasks)
  (gen-all-rook-magics)
  (gen-all-bishop-magics)
  (gen-obstructed-rook)
  (gen-obstructed-bishop)
  (initialize-zobrist-keys))
