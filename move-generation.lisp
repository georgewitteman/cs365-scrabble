
;; =====================
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



;; FIND-BEST-MOVE
;; ---------------------
;; INPUT: GAME, a SCRABBLE struct
;; OUTPUT: A list containing a string and a list of positions

(defun find-best-move (game)
  (let ((move-list (generate-moves game))
	(gamey (copy-game game))
	(board (scrabble-board game))
	(best-move ())
	(best-score 0)
	(curr-score 0)
	(movie-len 0)
	(letter 0)
	(r 0)
	(c 0)
	(a-tile 0)
	(locs-list ())
	(tiles-list ())
	)
    (dolist (movie move-list)
      (setf movie-len (length movie))
      
      (dotimes (i movie-len)
	
	(setf letter (nth i (first movie)))
	(setf r (first (nth i (second movie))))
	(setf c (second (nth i (second movie))))

	;; Check if each letter already on board
	(when (empty-space? board r c)
	  ;; Create and add this tile to TILES-LIST
	  (setf a-tile (make-tile :letter letter
				  :value (svref *letter-val-array* (position letter *letters-array*))
				  :row r
				  :col c))
	  (cons a-tile tiles-list)))
      
      ;; Do-move! on GAMEY (copy of game)
      (do-move! gamey nil (first movie) (second movie))      
      ;; Score-word
      (setf curr-score (score-word (scrabble-board gamey) tiles-list))
      ;; Update best score
      (when (or (> curr-score best-score) (and (= curr-score best-score) 
					     (> (length (second movie))
						(length (second best-move)))))
	(setf best-move movie)
	(setf best-score curr-score))
      (setf gamey (copy-game game)))
    best-move))

;; DO-BEST-MOVE!
;; ------------------------
;; INPUT: GAME, a Scrabble struct
;; OUTPUT: GAME modified so that best move is played


(defun do-best-move! (game)
  (let ((movie (find-best-move game)))
    (do-move! game nil (first movie) (second movie))))


;; DO-RANDOM-MOVE!
;; ----------------------
;; INPUT: GAME, a Scrabble stuct
;; OUTPUT: GAME modified so that a random move is played

(defun do-random-move! (game)
  (let* ((move-list (generate-moves game))
	 (num-moves (length move-list))
	 (randy (random num-moves))
	 (movie (nth randy move-list)))
    (do-move! game nil (first movie) (second movie))))


	
;; MOVIE = ("string" ((r c) (r c) (r c)))
;; MOVIE-S = (tile_1 tile_2 ... tile_n) **not including tiles on board
;; MOVIE-DM = "string" and ((r c) (r c) (r c))