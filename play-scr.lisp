;; ========================
;; PLAY-SCR.LISP
;; ========================

;;  DO-MOVE! 
;; ------------------------
;;  INPUTS: GAME, a SCRABBLE struct to modify with the move
;;          WORD, a list of lists. The outer list forms the word, the inner
;;                list forms a tile


;;  SCORE
;; ------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board. Note that
;;                 the NEW-TILES should already be placed on this board
;;          NEW-TILES, a LIST of the locations of the tiles that were
;;                     just placed on BOARD
;;  OUTPUT: A number of points for a given turn

(defun score (board new-tiles)
  (score-words board (make-words-locs board new-tiles)))

;;  MAKE-WORDS-LOCS
;; ----------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          NEW-TILES, A LIST of the locations of the newly placed tiles
;;  OUTPUTS: A LIST of the locations of all the newly created words

(defun make-words-locs (board new-tiles)
  (make-words-locs-acc board new-tiles nil))

;;  MAKE-WORDS-LOCS-ACC
;; --------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          NEW-TILES, A LIST of the locations of the newly placed tiles
;;  OUTPUTS: The locations of all the words made by NEW-TILES without
;;           any duplicates

(defun make-words-locs-acc (board new-tiles acc)
  (cond ((null new-tiles) (remove-duplicates acc :test #'equal))
        (t (make-words-locs-acc
             board
             (rest new-tiles)
             (append acc (get-word-locs
                           board
                           (first (first new-tiles))
                           (second (first new-tiles))))))))

;;  GET-WORD-MULTIPLIER
;; --------------------------
;;  INPUTS: WORD-LOCS, A list of locations for a word
;;  OUTPUT: The multiplier for that word. For example, a word with a tile
;;          in (0 0) would return at least 3 (possibly 9+ if it covers another
;;          triple word space

(defun get-word-multiplier (word-locs)
  (get-word-multiplier-acc word-locs 1))

(defun get-word-multiplier-acc (word-locs acc)
  (if (null word-locs)
    acc
    (let* ((row (first (first word-locs)))
           (col (second (first word-locs)))
           (premium (get-space-premium row col)))
      (cond ((equal premium *dw*)
             (get-word-multiplier-acc (rest word-locs)
                                      (* acc 2)))
            ((equal premium *tw*)
             (get-word-multiplier-acc (rest word-locs)
                                      (* acc 3)))
            (t (get-word-multiplier-acc (rest word-locs) acc))))))

;;  GET-TILE-MULTIPLIER
;; -----------------------------
;;  INPUTS: ROW, COL, the row and col of the space to check
;;  OUTPUT: The multiplier for that space (i.e 3 for TL, 2 for DL)

(defun get-tile-multiplier (row col)
  (let ((premium (get-space-premium row col)))
    (cond ((equal premium *dl*) 2)
          ((equal premium *tl*) 3)
          (t 1))))

;;  GET-SPACE-PREMIUM
;; ------------------------------
;;  INPUTS: ROW, COL, the row and col of the space to check
;;  OUTPUT: The premium for that given space, NIL if no premium

(defun get-space-premium (row col)
  (let ((loc (aref *initial-board* row col)))
    (if (equal loc *open*)
      nil
      loc)))

;;  SCORE-WORD
;; ------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          WORD, A LIST of locations on the game board for a word
;;  OUTPUTS: The score for that word

(defun score-word (board word)
  (* (get-word-multiplier word) (score-word-acc board word 0)))

;;  SCORE-WORD-ACC
;; ------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          WORD, A LIST of locations on the game board for a word
;;          ACC, an accumulator for the score (adds score for a letter
;;               each time)
;;  OUTPUTS: The score for that word

(defun score-word-acc (board word acc)
  (cond ((null word) acc)
        (t (let* ((word-pos (first word))
                  (tile (tile-from-loc board
                                       (first word-pos)
                                       (second word-pos)))
                  (tile-index (position tile *letters-array*))
                  (tile-value (* (get-tile-multiplier (first word-pos)
                                                      (second word-pos))
                                 (svref *letter-val-array* tile-index))))
             ;Debugging...
             ;(format t "tile: ~A~%" tile)
             ;(format t "tile-value: ~A~%" tile-value)
             (score-word-acc board (rest word) (+ acc tile-value))))))

;;  SCORE-WORDS
;; -----------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          WORDS-LOCS, a list of words (itself a list of locations)
;;  OUTPUTS: The score for all of the words in WORDS-LOCS

(defun score-words (board words-locs)
  (score-words-acc board words-locs 0))

;;  SCORE-WORDS-ACC
;; -----------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          WORDS-LOCS, a list of words (itself a list of locations)
;;          ACC, accumulator for the score of the words
;;  OUTPUTS: The score for all of the words in WORDS-LOCS

(defun score-words-acc (board words-locs acc)
  (cond ((null words-locs) acc)
        (t (score-words-acc board 
                            (rest words-locs)
                            (+ (score-word board (first words-locs))
                               acc)))))

;;  GET-WORD-LOCS
;; -------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          ROW & COL, the row and column of a location on the board
;;  OUTPUT: A LIST of locations for all words from the given location

(defun get-word-locs (board row col)
  (if (empty-space? board row col)
    nil
    (let ((locs nil)
          (start-row row)
          (start-col col)
          (end-row row)
          (end-col col))
      ;; look to the left
      (while (not (or (off-board? board start-row start-col)
                      (empty-space? board start-row start-col)))
             (decf start-col))
      (incf start-col)
      ;; look to the right
      (while (not (or (off-board? board end-row end-col)
                      (empty-space? board end-row end-col)))
             (incf end-col))
      (decf end-col)
      ;; add to list if it's more than 1 char
      (when (not (equal start-col end-col))
        (setf locs (list (word-locs-from-start-end
                           (list start-row start-col)
                           (list end-row end-col)))))
      (setf start-row row)
      (setf start-col col)
      (setf end-row row)
      (setf end-col col)
      ;; look up
      (while (not (or (off-board? board start-row start-col)
                      (empty-space? board start-row start-col)))
             (decf start-row))
      (incf start-row)
      ;; look down
      (while (not (or (off-board? board end-row end-col)
                      (empty-space? board end-row end-col)))
             (incf end-row))
      (decf end-row)
      ;; add to list if it's more than 1 char
      (when (not (equal start-row end-row))
        (setf locs (append locs (list (word-locs-from-start-end
                                        (list start-row start-col)
                                        (list end-row end-col))))))
      locs)))

;;  WORD-LOCS-FROM-START-END
;; --------------------------------
;;  INPUT: START-LOC, END-LOC, A list containing the row and col
;;                             for the start & end of a word
;;  OUTPUT: A list containing locations (list row col) of all the
;;          letters in a word

(defun word-locs-from-start-end (start-loc end-loc)
  (let ((start-row (first start-loc))
        (start-col (second start-loc))
        (end-row (first end-loc))
        (end-col (second end-loc))
        (locs nil))
    (if (equal start-row end-row)
      ;; Horizontal
      (while (<= start-col end-col)
             (setf locs (append locs
                                (list (list start-row start-col))))
             (incf start-col))

      ;; Vertical
      (while (<= start-row end-row)
             (setf locs (append locs
                                (list (list start-row start-col))))
             (incf start-row))
      )
    locs))


;;  OFF-BOARD?
;; ------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          ROW, COL, a possible tile position
;;  OUTPUT: T if (ROW, COL) is off the board, NIL otherwise

(defun off-board? (board row col)
  (let ((dims (array-dimensions board)))
    (or (< row 0)
        (< col 0)
        (> row (first dims))
        (> col (second dims)))))

;;  EMPTY-SPACE?
;; -------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          ROW, COl, a possible tile position
;;  OUTPUT: T if (ROW, COL) is empty in GAME, NIL otherwise

(defun empty-space? (board row col)
  (let ((loc (aref board row col)))
    (or (equal loc *open*)
        (equal loc *tw*)
        (equal loc *dl*)
        (equal loc *dw*)
        (equal loc *tl*))))

;;  TILE-FROM-LOC
;; ------------------------
;;  INPUTS: BOARD, a 2D array representing a SCRABBLE board
;;          ROW & COL, row and col of the tile
;;  OUTPUT: A symbol representing the tile that's placed. NIL if there's no
;;          tile at the given position

(defun tile-from-loc (board row col)
  (if (empty-space? board row col)
    nil
    (read-from-string (string
                        (aref (aref board row col) 1)))))

;; PLACE-TILE!
;; ------------------------
;; INPUT: GAME, a SCRABBLE struct, LETTER, a letter, ROW, COL
;; SIDE EFFECT: Place tile on board

(defun place-tile! (game letter row col)
  (let ((*string*  (string letter)))
    (setf *string* (concatenate 'string " " *string*))
    (setf (aref (scrabble-board game) row col) *string*)))



;; REMOVE-FROM-RACK! 
;; ------------------------
;; INPUT: GAME, a SCRABBLE struct, LETTER, a letter
;; OUTPUT: A tile of that LETTER 
;; SIDE EFFECT: Modifies GAME by removing tile from the rack

(defun remove-from-rack! (game letter)
  (let ((player (whose-turn game))
	(index 0)
	(tile (make-tile :letter letter
			 :value (svref *letter-val-array* (position letter *letters-array*)))))
    (setf (scrabble-rack_0 game) 
      (remove tile (scrabble-rack_0 game) :test #'tile-eq? :count 1))))
;	(cond 
;	 ((equal player *ply0*)
;	  (setf index (position letter (scrabble-rack_0 game))) 
;;	  (setf tile (nth index (scrabble-rack_0 game)))
;	  (remove letter (scrabble-rack_0 game) :count 1))
;;	 (T
;	  (setf index (position letter (scrabble-rack_1 game)))
;	  (setf tile (nth index (scrabble-rack_1 game)))
;	  (remove letter (scrabble-rack_1 game) :count 1)))))
	 ;; PLAYER 1
      ;;(remove letter (scrabble-rack_1 game) :count 1)
    
    

;; PICK-TILES!
;; ------------------------
;; INPUT: GAME, scrabble struct, PLAYER, either *ply0* or *ply1*, N, number
;; OUTPUT: GAME, modified such that N tiles are added to PLAYER's rack

(defun pick-tiles! (game player n)
  (dotimes (i n)
    (if (equal player *ply0*)
	;; Player 0
	(setf (scrabble-rack_0 game)
	  (cons (first (scrabble-bag game))
		(scrabble-rack_0 game)))
      ;; Player 1
      (setf (scrabble-rack_1 game)
	(cons (first (scrabble-bag game))
	      (scrabble-rack_1 game))))
    ;; Remove tile from bag
    (setf (scrabble-bag game)
      (rest (scrabble-bag game)))))