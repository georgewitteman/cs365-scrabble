;;  MOVE-GENERATION.LISP
;; =====================

;;  COMPUTE-CROSS-CHECKS
;; --------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;  OUTPUT: A 2D array where each element is a list containing all
;;          legal letters that can be placed in that space

(defun compute-cross-checks (board)
  (let ((checks (make-array '(15 15) :initial-element nil)))
    (dotimes (row 15)
      (dotimes (col 15)
        (if (is-anchor? board row col)
          (setf (aref checks row col)
                (cross-checks-space board row col))
          (setf (aref checks row col)
                '---))))
    checks))

;;  CROSS-CHECKS-SPACE
;; ---------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          ROW & COL, location of the space to check
;;  OUTPUT: A list of CHARACTERs that are can be placed at the given space

(defun cross-checks-space (board row col)
  ;; Find word above & below
  (let ((checks nil)
        (word-above (tiles-to-chars (get-word-above board row col)))
        (word-below (tiles-to-chars (get-word-below board row col))))
    (if (and (null word-above) (null word-below))
      (setf checks '+++)
      )
    ;; Check the dictionary for what letters can be
    ;; between word-above & word-below
    ;(dolist (letter *letters-list*))
    ;(when (in-trie? *trie* (append word-above (list letter) word-below))
    ;(cons letter checks))
    checks))

;;  GET-VERTICAL-WORD
;; -------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          ROW & COL, the space to start searching
;;          DELT, the direction to search (-1 for up, 1 for down)
;;  OUTPUT: A list containing the tiles either above or below
;;          the given tile (inclusive)

(defun get-vertical-word (board row col delt)
  (get-vertical-word-acc board row col delt nil))

(defun get-vertical-word-acc (board row col delt acc)
  (cond ((empty-space? board row col) acc)
        (t (get-vertical-word-acc
             board
             (+ row delt)
             col delt
             (cons (aref board row col) acc)))))
  
;; Some useful helpers..
(defun get-word-above (board row col)
  (get-vertical-word board (1- row) col -1))

(defun get-word-below (board row col)
  ;; Reverse to keep the right ordering of letters
  (reverse (get-vertical-word board (1+ row) col 1)))

;;  TILES-TO-CHARS
;; -----------------------
;;  INPUTS: TILES, a LIST of TILES
;;  OUTPUT: A list of characters

(defun tiles-to-chars (tiles)
  (map 'list #'tile-letter tiles))


;;  FIND-ANCHORS
;; --------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;  OUTPUT: A list of locations ('(row col)) of all anchors on BOARD

(defun find-anchors (board)
  (let ((anchors nil))
    (dotimes (row 15)
      (dotimes (col 15)
        (when (is-anchor? board row col)
          (setf anchors (append anchors (list (list row col)))))))
    anchors))

;;  IS-ANCHOR?
;; --------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          ROW & COL, the position of the space to check
;;  OUTPUTS: T if the space is an anchor, NIL otherwise

(defun is-anchor? (board row col)
  (and (empty-space? board row col)
       (not (and (empty-space? board row (1+ col))
                 (empty-space? board row (1- col))
                 (empty-space? board (1+ row) col)
                 (empty-space? board (1- row) col)))))


