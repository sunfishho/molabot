

(defvar *transposition-table* (make-hash-table))

;;;flag is 0 if exact, 1 if lower bound, and 2 if upper bound
(defun add-eval-depth (hash eval move depth flag)
  (setf (gethash hash *transposition-table*) (list depth eval move flag)))

(defmacro reset-transposition-table ()
  `(setf *transposition-table* (make-hash-table)))
