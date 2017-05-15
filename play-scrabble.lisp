;; ========================
;; PLAY-SCR.LISP
;; ========================

;;  DO-RANDOM-MOVE!
;; ----------------------

(defun do-random-move! (game)
  (let* ((moves (generate-moves game))
         (move (nth (random (length moves)) moves))
         (word (first move))
         (locs (second move)))
    (do-move! g t word locs))
  game)


;;  DO-MOVE!
;; ----------------------
;;  INPUTS: GAME, a SCRABBLE struct
;;          CHECK-LEGAL?, T if we should check if the move is legal
;;          WORD, a STRING representing the letters (not on board yet)
;;          LOCS, a list of locations (not including on board)
;;  OUTPUTS: The score for the given move
;;  SIDE-EFFECT: Modifies GAME to include TILES on the board and modifies
;;               each TILE to include it's position

(defun do-move! (game check-legal? word locs)
  (let ((new-tiles (get-new-tiles game word locs))
        (new-locs (get-new-locs game locs)))
    (if (or (not check-legal?)
            (is-legal? game word locs))
      (progn 
        (place-all-tiles! (scrabble-board game) new-tiles new-locs)
        (refill-racks! game)
        (format t "tiles:: ~A~%" new-tiles)
        (let ((score (score (scrabble-board game) new-tiles)))
          (incf (svref (scrabble-score game) (whose-turn game))
                score)
          (setf (scrabble-whose-turn game) (- 1 (whose-turn game)))
          score))
      (format t "INVALID MOVE!~%"))))

(defun get-new-locs (game locs)
  (get-new-locs-acc game locs nil))

(defun get-new-locs-acc (game locs acc)
  (cond ((null locs) acc)
        ((empty-space? (scrabble-board game)
                       (first (first locs))
                       (second (first locs)))
         (get-new-locs-acc game (rest locs)
                           (append acc (list (first locs)))))
        (t (get-new-locs-acc game (rest locs) acc))))


(defun get-new-tiles (game word locs)
  (get-new-tiles-acc game (coerce word 'list) locs nil))

(defun get-new-tiles-acc (game word locs acc)
  (cond ((null word) acc)
        ((empty-space? (scrabble-board game)
                       (first (first locs))
                       (second (first locs)))
         (get-new-tiles-acc game (rest word) (rest locs)
                            (append acc (list (get-from-rack game
                                                              (first word))))))
        (t (get-tiles-move-acc game (rest word) (rest locs) acc))))
        ;(t (get-tiles-move-acc game (rest word) (rest locs)
                               ;(append acc (list (tile-from-loc
                                                   ;(scrabble-board g)
                                                   ;(first (first locs))
                                                   ;(second (first locs)))))))))

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
        (t (when (empty-space? board (first (first locs)) (second (first locs)))
             (place-tile! board
                          (first tiles)
                          (first (first locs))
                          (second (first locs)))
             (remove-from-rack! g (first tiles)))
           (place-all-tiles! board (rest tiles) (rest locs)))))

;;  IS-LEGAL?
;; ---------------------------
;;  INPUTS: GAME
;;          TILES
;;          LOCS
;;  OUTPUTS: t if the move is legal, NIL otherwise

(defun is-legal? (game word locs)
  (if (empty-board? (scrabble-board game))
    (and (valid-first-word? locs)
         (is-word? word *trie*))
    (is-word? word *trie*)))

;;  VALID-FIRST-WORD?
;; --------------------------
;;  INPUTS: LOCS, a LIST of LISTS of 2 integers representing the locations
;;          to place the TILEs
;;  OUTPUT: T if the LOCS contain (7 7)

(defun valid-first-word? (locs)
  (cond ((null locs) nil)
        ((equal (first locs) '(7 7)) t)
        (t (valid-first-word? (rest locs)))))

;;  IS-A-WORD?
;; -------------------------
;;  INPUTS: WORD, a LIST of TILEs representing a possible word
;;  OUTPUT: t if the WORD is in the dictionary, NIL otherwise

(defun is-a-word? (word)
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
        (t 
          ;(format t "first new tiles: ~A~%" (first new-tiles))
          (get-new-words-acc
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

;;  OFF-BOARD?
;; ------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          ROW, COL, a possible tile position
;;  OUTPUT: T if (ROW, COL) is off the board, NIL otherwise

(defun off-board? (board row col)
  (let ((dims (array-dimensions board)))
    (or (< row 0)
        (< col 0)
        (>= row (first dims))
        (>= col (second dims)))))

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
;;  OUTPUT: The modified tile
;;  SIDE-EFFECTS: Modifies the tile to contain it's location,
;;                and modifies the board to contain the tile

(defun place-tile! (board tile row col)
  (setf (tile-row tile) row)
  (setf (tile-col tile) col)
  (setf (aref board row col) tile)
  tile)

;;  GET-FROM-RACK
;; -------------------
;;  INPUTS: GAME, a SCRABBLE struct
;;          LETTER, a CHARACTER
;;  OUTPUT: A tile that matches LETTER in the current players rack

(defun get-from-rack (game letter)
  (when (in-rack? game letter)
    (make-tile :letter (char-upcase letter)
               :value (svref *letter-val-array*
                             (position letter *letters-array*
                                       :test #'char-equal)))))

;;  GET-RACK
;; ---------------------
;;  INPUTS: GAME, a SCRABBLE struct
;;  OUTPUT: The current players rack

(defun get-rack (game)
  (if (= (whose-turn game) 0)
    (scrabble-rack_0 game)
    (scrabble-rack_1 game)))

;;  IN-RACK?
;; --------------------
;;  INPUT: GAME, a SCRABBLE struct
;;         LETTER, a CHARACTER
;;  OUTPUT: T if the LETTER is in the rack, NIL otherwise

(defun in-rack? (game letter)
  (let ((rack (if (= (whose-turn game) 0)
                (scrabble-rack_0 game)
                (scrabble-rack_1 game)))
        (tile (make-tile :letter (char-upcase letter)
                         :value 0)))
    (not (null (position tile rack :test #'tile-eq?)))))

;;  PLACE-IN-RACK!
;; -----------------------
;;  INPUTS: GAME, a SCRABBLE struct
;;          TILE, a TILE struct
;;  OUTPUT: The modified rack
;;  SIDE-EFFECT: Modifies the rack for the current player to contain TILE

(defun place-in-rack! (game tile)
  (if (= (whose-turn game) 0)
    (setf (scrabble-rack_0 game)
          (cons tile (scrabble-rack_0 game)))
    (setf (scrabble-rack_1 game)
          (cons tile (scrabble-rack_1 game)))))

;;  REMOVE-FROM-RACK! 
;; ------------------------
;;  INPUT: GAME, a SCRABBLE struct
;;         TILE, a TILE struct
;;  OUTPUT: A tile of that LETTER 
;;  SIDE EFFECT: Modifies GAME by removing tile from the rack

(defun remove-from-rack! (game tile)
  (if (= (whose-turn game) 0)
    (setf (scrabble-rack_0 game)
          (remove tile (scrabble-rack_0 game) :test #'tile-eq? :count 1))
    (setf (scrabble-rack_1 game)
          (remove tile (scrabble-rack_1 game) :test #'tile-eq? :count 1)))
    tile)

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
  (setf (scrabble-num-tiles-left game)
    (- (scrabble-num-tiles-left game) n))
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


;; TRADE-IN!
;; ---------------------
;; INPUT: GAME, a scrabble struct, LISTY, list of numbers corresponding to tiles in rack
;; OUTPUT: GAME, modified so that player has traded in specified tiles
;; SIDE EFFECT: Modifies GAME

(defun trade-in! (game listy)
  (let* ((initial-rack (scrabble-rack_0 game))
	 (len (length initial-rack))
	 (lis-len (length listy))
	 (new-rack '()))
    
    ;; If it is Player 1's turn, reset rack
    (when (equal (whose-turn game) *ply1*)
      (setf initial-rack (scrabble-rack_1 game))
      (setf len (length initial-rack)))
    
    ;; Add each letter in rack to bag or NEW-RACK
    (dotimes (i len)
      (when (not (equal i (first listy)))
	(setf new-rack (cons (nth i initial-rack) new-rack)))
      (when (equal i (first listy))
	(setf listy (rest listy))
	(setf (scrabble-bag game) (cons (nth i initial-rack) (scrabble-bag game)))))
    
    ;; Set NEW-RACK
    (if (equal (whose-turn game) *ply0*)
	(setf (scrabble-rack_0 game) new-rack)
      (setf (scrabble-rack_1 game) new-rack))

    ;; Shake the bag
    (shake-bag! game)
    
    ;; Put new tiles into the rack
    (pick-tiles! game (whose-turn game) lis-len)

    ;; Print scrabble
    (print-scrabble game t 0)
    
    ;; Toggle turns
    (pass! game)
    
    game))


;; PASS!
;; -------------------------
;; INPUT: GAME, a scrabble struct
;; OUTPUT: GAME after player has passed
;; SIDE EFFECT: Switch turn

(defun pass! (game)
  (let ((player (whose-turn game)))
    (if (equal player *ply0*)
	(setf (scrabble-whose-turn game) *ply1*)
      (setf (scrabble-whose-turn game) *ply0*))
    game))
