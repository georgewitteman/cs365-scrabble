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

;;  GET-HORIZONTAL-WORD
;; -------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          ROW & COL, the space to start searching
;;          DELT, the direction to search (-1 for left, 1 for right)
;;  OUTPUT: A list containing the tiles either above or below
;;          the given tile (inclusive)

(defun get-horizontal-word (board row col delt)
  (get-horizontal-word-acc board row col delt nil))

(defun get-horizontal-word-acc (board row col delt acc)
  (cond ((empty-space? board row col) acc)
        (t (get-horizontal-word-acc
             board
             row
             (+ col delt)
             delt
             (cons (aref board row col) acc)))))

;; Some useful helpers...
(defun get-word-left (board row col)
  (get-horizontal-word board row (1- col) -1))

(defun get-word-right (board row col)
  (reverse (get-horizontal-word board row (1+ col) 1)))

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

;;  GENERATE-MOVES
;; -----------------------
;;  INPUTS: GAME, a SCRABBLE struct
;;  OUTPUT: A list of MOVEs that the current player can do

(defun generate-moves (game)
  (let ((board (scrabble-board g)))
    ;; For each anchor
    (dolist (anchor (find-anchors board))
      (let ((row (tile-row anchor))
            (col (tile-col anchor)))
        (if (not (empty-space? board (1- row) col))
          ;; If the left part is already on the board
          (let ((left (get-word-left board row col)))
            ;; extend right
            )
          ;; Create all possible left parts
          ;(left-part nil root-node (get-limit board row col))
          )))))

;;  GET-LIMIT
;; ---------------------
;;  INPUTS: BOARD, a 2d array representing a scrabble board
;;          ROW & COL, the position of the location to get limit for
;;  OUTPUT: The limit, i.e the number of non-anchor squares to the
;;          left of the given space

(defun get-limit (board row col)
  (get-limit-acc board row (1- col) 0))

(defun get-limit-acc (board row col acc)
  (cond ((is-anchor? board row col) acc)
        ((< col 0) acc)
        (t (get-limit-acc board row (1- col) (1+ acc)))))























