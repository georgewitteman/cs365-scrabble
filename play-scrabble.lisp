;; ========================
;; PLAY-SCR.LISP
;; ========================

;;  DO-MOVE!
;; ----------------------
;;  INPUTS: GAME, a SCRABBLE struct
;;          CHECK-LEGAL?, T if we should check if the move is legal
;;          WORD, a STRING representing the letters to put down
;;          LOCS, a list of locations
;;  OUTPUTS: The score for the given move
;;  SIDE-EFFECT: Modifies GAME to include TILES on the board and modifies
;;               each TILE to include it's position

(defun do-move! (game check-legal? word locs)
  (let ((tiles (tiles-from-string game word)))
    (if (or (not check-legal?)
            (is-legal? game tiles locs))
      (progn 
        (place-all-tiles! (scrabble-board game) tiles locs)
        (refill-racks! game)
        (let ((score (score (scrabble-board game) tiles)))
          (incf (svref (scrabble-score game) (whose-turn game))
                score)
          (setf (scrabble-whose-turn game) (- 1 (whose-turn game)))
          score))
      (format t "INVALID MOVE!~%"))))

;;  PLACE-ALL-TILES!
;; -----------------------
;;  INPUTS: BOARD, a 2D array rpresenting a scrabble board.
;;          TILES, a LIST of TILEs
;;          LOCS, a LIST of LISTS of 2 integers representing the locations
;;                to place the TILEs
;;  OUTPUTS: The modified board
;;  SIDE-EFFECT: Modifies BOARD with TILEs at LOCS

(defun place-all-tiles! (board tiles locs)
  (cond ((null tiles) board)
        (t (place-tile! board
                        (first tiles)
                        (first (first locs))
                        (second (first locs)))
           (place-all-tiles! board (rest tiles) (rest locs)))))

;;  IS-LEGAL?
;; ---------------------------
;;  INPUTS: GAME
;;          TILES
;;          LOCS
;;  OUTPUTS: t if the move is legal, NIL otherwise

(defun is-legal? (game tiles locs)
  (if (empty-board? (scrabble-board game))
    (and (valid-first-word? locs)
         (is-word? tiles))
    (is-word? tiles)))

;;  VALID-FIRST-WORD?
;; --------------------------
;;  INPUTS: LOCS, a LIST of LISTS of 2 integers representing the locations
;;          to place the TILEs
;;  OUTPUT: T if the LOCS contain (7 7)

(defun valid-first-word? (locs)
  (cond ((null locs) nil)
        ((equal (first locs) '(7 7)) t)
        (t (valid-first-word? (rest locs)))))

;;  IS-WORD?
;; -------------------------
;;  INPUTS: WORD, a LIST of TILEs representing a possible word
;;  OUTPUT: t if the WORD is in the dictionary, NIL otherwise

(defun is-word? (word)
  (let ((word-str ""))
    (dolist (til word)
      (setf word-str (concatenate 'string word-str (string (tile-letter til)))))
    (if (null (position word-str *ospd* :test #'word-equal?))
      nil
      t)))

;; Some Tests
;; (is-word? (list
;;   (make-tile :letter #\A :row 0 :col 0) 
;;   (make-tile :letter #\A :row 1 :col 0)
;;   (make-tile :letter #\H :row 2 :col 0)))

;;  EMPTY-BOARD?
;; ---------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;  OUTPUT: T if the board doesn't have any tiles, NIL otherwise

(defun empty-board? (board)
  (dotimes (row 15)
    (dotimes (col 15)
      (when (not (empty-space? board row col))
        (return-from empty-board? nil))))
  t)


;;  WORD-EQUAL?
;; ---------------------
;;  INPUTS: WORD1 & WORD2, two STRINGS
;;  OUTPUT: t if the words represent the same word, NIL otherwise

(defun word-equal? (word1 word2)
  (when (string-equal word1 word2) (return-from word-equal? t))
  (when (not (= (length word1) (length word2))) (return-from word-equal? nil))
  (dotimes (n (length word1))
    (when (not (or (char-equal (aref word1 n) (aref word2 n))
                   (char-equal #\- (aref word1 n))
                   (char-equal #\- (aref word2 n))))
      (return-from word-equal? nil)))
  t)

;; Some Tests
;; (word-equal? "" "")
;; (word-equal? "abcde" "abcde")
;; (word-equal? "abcde" "abcdd")
;; (word-equal? "abcde" "abcd")
;; (word-equal? "abcde" "ab-de")

;;  SCORE
;; ------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board. Note that
;;                 the NEW-TILES should already be placed on this board
;;          NEW-TILES, a list of TILE structs representing tiles
;;                     on the board
;;  OUTPUT: The score for the given move

(defun score (board new-tiles)
  (score-words board (get-new-words board new-tiles)))

;;  SCORE-WORDS
;; --------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          WORDS, A LIST containing a LIST of TILEs representing words
;;  OUTPUT: The total score for all the given words

(defun score-words (board words)
  (score-words-acc board words 0))

(defun score-words-acc (board words score)
  (cond ((null words) score)
        (t (score-words-acc board
                            (rest words)
                            (+ (* (score-word board (first words))
                                  (get-word-multiplier (first words)))
                               score)))))

;;  SCORE-WORD
;; ----------------------------
;;  INPUTS: BOARD, a 2D array represnting a scrabble board
;;          WORD, a LIST of TILEs representing a word
;;  OUTPUT: The score for the given word

(defun score-word (board word)
  (when *debugging*
    (format t "score for ~A:~A ~%" word (score-word-acc board word 0)))
  (score-word-acc board word 0))

(defun score-word-acc (board word score)
  (cond ((null word) score)
        (t (score-word-acc board
                           (rest word)
                           (+ score
                              (* (get-tile-multiplier (tile-row (first word))
                                                      (tile-col (first word)))
                                 (tile-value (first word))))))))

;;  GET-TILE-MULTIPLIER
;; ------------------------
;;  INPUTS: ROW & COL, position of space on the board
;;  OUTPUT: The multiplier for that space

(defun get-tile-multiplier (row col)
  (let ((premium (get-space-premium row col)))
    (cond ((equal premium *dl*) 2)
          ((equal premium *tl*) 3)
          (t 1))))

;;  GET-WORD-MULTIPLIER
;; ------------------------
;;  INPUTS: WORD, A LIST of TILEs representing a word on the board
;;  OUTPUT: The multiplier for that word. For example, a word with a tile
;;          in (0 0) would return at least 3 (possibly 9+ if it covers another
;;          triple word space.

(defun get-word-multiplier (word)
  (get-word-multiplier-acc word 1))

(defun get-word-multiplier-acc (word acc)
  (if (null word)
    acc
    (let* ((row (tile-row (first word)))
           (col (tile-col (first word)))
           (premium (get-space-premium row col)))
      (cond ((equal premium *dw*)
             (get-word-multiplier-acc (rest word)
                                      (* acc 2)))
            ((equal premium *tw*)
             (get-word-multiplier-acc (rest word)
                                      (* acc 3)))
            (t (get-word-multiplier-acc (rest word) acc))))))

;;  GET-SPACE-PREMIUM
;; ------------------------------
;;  INPUTS: ROW, COL, the row and col of the space to check
;;  OUTPUT: The premium for that given space, NIL if no premium

(defun get-space-premium (row col)
  (let ((loc (aref *initial-board* row col)))
    (if (equal loc *open*)
      nil
      loc)))

;;  GET-NEW-WORDS
;; --------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          NEW-TILES, a LIST of TILEs representing the newly placed tiles
;;  OUTPUTS: A LIST containing a LIST of TILEs representing all the newly
;;           created words

(defun get-new-words (board new-tiles)
  (get-new-words-acc board new-tiles nil))

(defun get-new-words-acc (board new-tiles acc)
  (cond ((null new-tiles) (remove-duplicates acc :test #'equal))
        (t (get-new-words-acc
             board
             (rest new-tiles)
             (append acc
                     (get-words-at-tile board (first new-tiles)))))))

;;  GET-WORDS-AT-TILE
;; -----------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          TILE, a TILE on the BOARD
;;  OUTPUT: A LIST of LISTs of TILEs representing all words that are made
;;          from the given TILE, NIL if no words are made

(defun get-words-at-tile (board tile)
  (let ((words nil)
        (horizontal (get-word-from-start-end
                      board
                      (get-end-tile board tile -1 0)
                      (get-end-tile board tile 1 0)))
        (vertical (get-word-from-start-end
                    board
                    (get-end-tile board tile 0 -1)
                    (get-end-tile board tile 0 1))))
    (if (> (length horizontal) 1)
      (setf words (append words (list horizontal))))
    (if (> (length vertical) 1)
      (setf words (append words (list vertical))))
    words))

;;  GET-WORD-FROM-START-END
;; ------------------------------
;;  INPUTS: START-TILE, a TILE that begins a word
;;          END-TILE, a TILE that ends a word
;;  OUTPUT: A LIST of all the TILEs from START-TILE to END-TILE inclusive

(defun get-word-from-start-end (board start-tile end-tile)
  (let ((start-row (tile-row start-tile))
        (start-col (tile-col start-tile))
        (end-row (tile-row end-tile))
        (end-col (tile-col end-tile))
        (locs nil))
    (if (equal start-row end-row)
      ;; Horizontal
      (while (<= start-col end-col)
             (setf locs (cons (tile-from-loc board start-row start-col)
                              locs))
             (incf start-col))
      ;; Vertical
      (while (<= start-row end-row)
             (setf locs (cons (tile-from-loc board start-row start-col)
                              locs))
             (incf start-row)))
    (reverse locs)))

;;  GET-END-TILE
;; -----------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          TILE, a TILE on the board
;;          D-HORIZ & D-VERT, how many spaces to check in each direction (+/-1)
;;  OUTPUT: the first or last TILE in a row of tiles

(defun get-end-tile (board tile d-horiz d-vert)
  (let ((prev-row (tile-row tile))
        (prev-col (tile-col tile))
        (row (tile-row tile))
        (col (tile-col tile)))
    (while (not (or (off-board? board row col)
                    (empty-space? board row col)))
           (setf prev-row row)
           (setf prev-col col)
           (setf row (+ row d-vert))
           (setf col (+ col d-horiz)))
    (tile-from-loc board prev-row prev-col)))

;;  WORD-TILES-FROM-START-END
;; -------------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          START, a lis

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
  (if (or (>= row 15) (< row 0) (>= col 15) (< col 0))
    t
    (let ((loc (aref board row col)))
      (or (equal loc *open*)
          (equal loc *tw*)
          (equal loc *dl*)
          (equal loc *dw*)
          (equal loc *tl*)))))

;;  TILE-FROM-LOC
;; ------------------------
;;  INPUTS: BOARD, a 2D array representing a SCRABBLE board
;;          ROW & COL, row and col of the tile
;;  OUTPUT: The TILE at the given position on BOARD

(defun tile-from-loc (board row col)
  (if (empty-space? board row col)
    nil
    (aref board row col)))

;;  PLACE-TILE!
;; ----------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          TILE, a TILE struct
;;          ROW & COL, the location on the board to place the tile
;;  OUTPUTS: The modified tile
;;  SIDE-EFFECTS: Modifies the tile to contain it's location,
;;                and modifies the board to contain the tile

(defun place-tile! (board tile row col)
  (setf (tile-row tile) row)
  (setf (tile-col tile) col)
  (setf (aref board row col) tile)
  tile)


;;  REMOVE-FROM-RACK! 
;; ------------------------
;;  INPUT: GAME, a SCRABBLE struct, LETTER, a CHARACTER
;;  OUTPUT: A tile of that LETTER 
;;  SIDE EFFECT: Modifies GAME by removing tile from the rack

(defun remove-from-rack! (game letter)
  (let ((rack (if (= (whose-turn game) 0)
                (scrabble-rack_0 game)
                (scrabble-rack_1 game)))
        (tile (make-tile :letter (char-upcase letter)
                         :value (svref *letter-val-array*
                                       (position letter *letters-array*
                                                 :test #'char-equal)))))
    (delete tile rack :test #'tile-eq? :count 1)
    tile))

;;  REFILL-RACKS!
;; -----------------------
;;  INPUTS: GAME, a SCRABBLE struct
;;  OUTPUT: The modified GAME (for convenience)
;;  SIDE-EFFECT: Refills the racks for both players in GAME

(defun refill-racks! (game)
  ;; Player 0
  (pick-tiles! game *ply0* (- 7 (length (scrabble-rack_0 game))))
  ;; Player 1
  (pick-tiles! game *ply1* (- 7 (length (scrabble-rack_1 game)))))

;;  PICK-TILES!
;; ------------------------
;;  INPUT: GAME, scrabble struct,
;;         PLAYER, either *ply0* or *ply1*,
;;         N, number
;;  OUTPUT: GAME, modified such that N tiles are added to PLAYER's rack
;;  SIDE-EFFECT: Modifies the appropriate rack in GAME

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

