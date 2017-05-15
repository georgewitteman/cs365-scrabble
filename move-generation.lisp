
;; =====================
;;  MOVE-GENERATION.LISP
;; =====================

(defparameter *legal-moves* nil)

(defparameter *trie* (new-trie))
(add-all-words *trie*)

(defun clear-legal-moves ()
  (setf *legal-moves* nil))

(defun add-legal-move (tiles locs transposed)
  ;(format t "A valid move:~%")
  (setf tiles (word-to-string tiles))
  ;(format t "~6T~A~%" tiles)
  ;(format t "~6T~A~%" (if transposed (transpose-locs locs) locs))
  (setf *legal-moves* (cons (list tiles (if transposed
                                          (transpose-locs locs)
                                          locs)) *legal-moves*)))

(defun transpose-locs (locs)
  (map 'list #'(lambda (loc) (list (second loc) (first loc))) locs))

;;  IN-CROSS-CHECK-SET?
;; ---------------------------
;;  INPUTS: BOARD, a scrabble board
;;          LETTER, a CHARACTER
;;          ROW & COL, space to check

(defun in-cross-check-set? (board letter row col)
  (not (null (member letter (cross-checks-space board row col)
                     :test #'char-equal))))

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
                *letters-list*))))
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
      (setf checks *letters-list*))
    ;; Check the dictionary for what letters can be
    ;; between word-above & word-below
    (dolist (letter *letters-list*)
      (when (is-word? (coerce (append
                                     word-above
                                     (list letter)
                                     word-below)
                                   'string)
                      *trie*)
        (setf checks (cons letter checks))))
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

(defun matrix-to-list (arr)
  (loop for i below (array-dimension arr 0)
        collect (loop for j below (array-dimension arr 1)
                      collect (aref arr i j))))

(defun list-to-2d-array (lst)
  (make-array (list (length lst)
                    (length (first lst)))
              :initial-contents lst))

(defun transpose-board! (game)
  (let ((board (scrabble-board game))
        (new-board (make-array '(15 15) :initial-element nil)))
    (dotimes (row 15)
      (dotimes (col 15)
        (setf (aref new-board row col) (aref board col row))))
    (dotimes (row 15)
      (dotimes (col 15)
        (if (not (empty-space? new-board row col))
          (let ((tile (aref new-board row col)))
            (setf (aref new-board row col)
                  (make-tile :letter (tile-letter tile)
                             :value (tile-value tile)
                             :row (tile-col tile)
                             :col (tile-row tile)))))))
    (setf (scrabble-board game) new-board))
  game)


;;  GENERATE-MOVES
;; -----------------------
;;  INPUTS: GAME, a SCRABBLE struct
;;  OUTPUT: A list of MOVEs that the current player can do

(defun generate-moves (game)
  (clear-legal-moves)
  (when (empty-board? (scrabble-board game))
    (extend-right game nil nil nil (get-root-node *trie*) '(7 7) nil)
    (return-from generate-moves *legal-moves*))
  (generate-moves-helper game nil)
  (transpose-board! game)
  (generate-moves-helper game t)
  (transpose-board! game)
  *legal-moves*)

(defun get-locs-from-word (word)
  (map 'list #'(lambda (tile) (list (tile-row tile) (tile-col tile))) word))

(defun generate-moves-helper (game transposed)
  (let ((board (scrabble-board game)))
    ;; For each anchor
    (dolist (anchor (find-anchors board))
      ;(format t "Generating moves for anchor ~A~%" anchor)
      (let ((row (first anchor))
            (col (second anchor)))
        (if (not (empty-space? board row (1- col)))
          ;; If the left part is already on the board
          (let ((left (get-word-left board row col)))
            (extend-right game
                          left
                          nil
                          (get-locs-from-word left)
                          (get-tr-node (word-to-string left) *trie*)
                          (list row col)
                          transposed))
          ;; Create all possible left parts
          (left-part game nil nil anchor (get-root-node *trie*)
                     (get-limit board row col)
                     transposed))))))

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

;;  LEFT-PART
;; -------------------------
;;  INPUTS: GAME, a SCRABBLE struct
;;          PARTIAL-WORD, a LIST of TILEs representing the already built part
;;                        of the word
;;          ANCHOR, the anchor square we are searching from
;;          NODEY, the node we get to from PARTIAL-WORD
;;          LIMIT, number of non-anchor squares next to the left of anchor
;;  OUTPUT: ???

(defun left-part (game partial-word partial-locs anchor nodey limit transposed)
  (extend-right game partial-word nil
                ;(append partial-locs (list (list 'L '])))
                partial-locs
                nodey anchor transposed)
  (if (> limit 0)
    ;; For each CHILD from NODEY
    (dolist (child (get-children nodey *trie*))
      ;; If the letter for CHILD is in our rack
      (when (in-rack? game (tr-node-char child))
        ;; Remove a tile corresponding to CHILD from our rack
        (let ((tile (remove-from-rack! game
                                       (get-from-rack game
                                                      (tr-node-char child)))))
          (left-part game
                     (append partial-word (list tile))
                     (if (null partial-locs)
                       (list (list (first anchor) (1- (second anchor))))
                       (cons (list (first (first partial-locs))
                                   (1- (second (first partial-locs))))
                             partial-locs))
                     anchor child (1- limit)
                     transposed)
          ;; Put the tile back into the rack
          (place-in-rack! game tile))))))

;;  EXTEND-RIGHT
;; ------------------------
;;  INPUTS: GAME, a SCRABBLE struct
;;          PARTIAL-WORD, a LIST of TILEs representing the already built part
;;                        of the word
;;          NODEY, the node we get to from PARTIAL-WORD
;;          SQUARE, the current square we're examining
;;  OUTPUTS: ??

(defun extend-right (game prefix partial-word partial-locs nodey square
                          transposed)
  (when (not (off-board? (scrabble-board game) (first square) (second square)))
    (if (and (empty-space? (scrabble-board game) (first square) (second square))
             (not (off-board? (scrabble-board game)
                              (first square)
                              (second square))))
      ;; if SQUARE is vacant then
      (progn (when (and (tr-node-is-word nodey)
                        (not (null partial-word)))
               ;; if NODEY is a terminal node then
               ;; LegalMove(PartialWord)
               (add-legal-move (append prefix partial-word) partial-locs
                               transposed))
             (dolist (child (get-children nodey *trie*))
               ;; If CHILD is in our rack...
               (if (and (in-rack? game (tr-node-char child))
                        ;; ...and CHILD is in the cross check set for SQUARE
                        (in-cross-check-set? (scrabble-board game)
                                             (tr-node-char child)
                                             (first square) (second square)))
                 (progn
                   ;; Remove the corresponding tile for CHILD from rack
                   (let ((tile (remove-from-rack!
                                 game
                                 (get-from-rack game
                                                (tr-node-char child))))
                         ;; Let the next square be the square to the
                         ;; right of square
                         (next-square (list
                                        (first square)
                                        (1+ (second square)))))
                     (extend-right game
                                   prefix
                                   (append partial-word (list tile))
                                   (append partial-locs
                                           (list
                                             (list (first square)
                                                   (second square))))
                                   child
                                   next-square
                                   transposed)
                     ;; Put the tile back into the rack
                     (place-in-rack! game tile))))))
      (progn
        (let ((tile (tile-from-loc
                      (scrabble-board game) (first square) (second square))))
          (when (not (null (get-child-char nodey (tile-letter tile) *trie*)))
            (let ((next-square (list (first square)
                                     (1+ (second square)))))
              (extend-right game
                            prefix
                            (append partial-word (list tile))
                            (append partial-locs
                                    (list (list (first square)
                                                (second square))))
                            (get-child-char nodey (tile-letter tile) *trie*)
                            next-square
                            transposed))))))))

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
	(format t "letter ~A r ~A c ~A~%" letter r c)
	;; Check if each letter already on board
	(when (empty-space? board r c)
	  ;; Create and add this tile to TILES-LIST
	  (setf a-tile (make-tile :letter letter
				  :value (svref *letter-val-array* (position letter *letters-array*))
				  :row r
				  :col c))
	  (format t "a-tile ~A~%" a-tile)
	  (cons a-tile tiles-list)))

      ;; Do-move! on GAMEY (copy of game)
      (do-move! gamey nil (first movie) (second movie))
      ;; Score-word
      (format t "best move ~A score ~A" best-move best-score)
      (setf curr-score (score-word (scrabble-board gamey) tiles-list))
      ;; Update best score
      (when (or (> curr-score best-score) (and (= curr-score best-score)
					     (> (length (second movie))
						(length (second best-move)))))
	(setf best-move movie)
	(setf best-score curr-score))
      (format t "best move ~A best score ~A~%" best-move best-score)
      (setf gamey (copy-game game)))
    best-move))

;; DO-BEST-MOVE!
;; ------------------------
;; INPUT: GAME, a Scrabble struct
;; OUTPUT: GAME modified so that best move is played


(defun do-best-move! (game)
  (let ((movie (find-best-move game)))
    (do-move! game nil (first movie) (second movie))))


;; MOVIE = ("string" ((r c) (r c) (r c)))
;; MOVIE-S = (tile_1 tile_2 ... tile_n) **not including tiles on board
;; MOVIE-DM = "string" and ((r c) (r c) (r c))
