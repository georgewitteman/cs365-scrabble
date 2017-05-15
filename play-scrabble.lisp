;; ========================
;; PLAY-SCR.LISP
;; ========================

;;  RANDOM-VS-BEST
;; ----------------------

(defun random-vs-best ()
  (let ((gm (new-scrabble)))
    (while (not (game-over? gm))
           (format t "~A~%" gm)
           (if (= (whose-turn gm) 0)
             (do-random-move! gm)
             (do-best-move! gm)))
    (format t "~A~%" gm)
    (if (> (svref (scrabble-score gm) 0) (svref (scrabble-score gm) 1))
      (format t "~%Player 1 WINS!~%")
      (format t "~%Player 2 WINS!~%"))))

;;  PLAY-RANDOM-GAME
;; -------------------
;;  INPUTS: NONE!
;;  OUTPUT: NIL
;;  SIDE-EFFECT: Creates and plays a random game

(defun play-random-game ()
  (let ((gm (new-scrabble)))
    (while (not (game-over? gm))
           (format t "~A~%" gm)
           (do-random-move! gm))
    (format t "~A~%" gm)
    (if (> (svref (scrabble-score gm) 0) (svref (scrabble-score gm) 1))
      (format t "~%Player 1 WINS!~%")
      (format t "~%Player 2 WINS!~%"))))

;;  DO-RANDOM-MOVE!
;; ----------------------
;;  INPUTS: GAME, a SCRABBLE struct
;;  OUTUPT: The modified game
;;  SIDE-EFFECT: Modifies the game with a random legal move

(defun do-random-move! (game)
  ;(format t "~A~%" game)
  (let* ((moves (generate-moves game))
         (move (nth (random (length moves)) moves))
         (word (first move))
         (locs (second move)))
    (if (null word)
      (pass! game)
      (do-move! game t word locs)))
  game)

;;  GAME-OVER?
;; --------------------
;;  INPUTS: GAME, a SCRABBLE struct
;;  OUTPUT: T if the game is over, NIL otherwise

(defun game-over? (game)
  (when (or (null (scrabble-rack_0 game))
            (null (scrabble-rack_1 game)))
    (return-from game-over? t))
  (when (null (generate-moves game))
    (pass! game)
    (if (null (generate-moves game))
      (return-from game-over? t)
      ;; Reset to original player
      (pass! game)))
  nil)

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
  ;(format t "Doing move: (player: ~A, word: ~A, locs: ~A)~%" 
          ;(if (= (whose-turn game) 0)
            ;"*plyr1*"
            ;"*plyr2*")
            ;word locs)
  (let ((new-tiles (get-new-tiles game word locs))
        (new-locs (get-new-locs game locs)))
    (when (or (not check-legal?)
              (is-legal? game word locs))
      (place-all-tiles! game new-tiles new-locs)
      (refill-racks! game)
      ;(format t "tiles:: ~A~%" new-tiles)
      (let ((score (score (scrabble-board game) new-tiles)))
        (incf (svref (scrabble-score game) (whose-turn game))
              score)
        (setf (scrabble-whose-turn game) (- 1 (whose-turn game)))
        (return-from do-move! score)))
    (format t "INVALID MOVE!~%")))

;;  GET-NEW-LOCS
;; --------------------------
;;  INPUTS: GAME, a SCRABBLE struct
;;          LOCS, a list of locations '(row col)
;;  OUTPUT: A LIST of locations where each location is from LOCS but not
;;          on the board of GAME

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

;;  GET-NEW-TILES
;; ----------------------------
;;  INPUTS: GAME, a SCRABBLE struct
;;          WORD, a list of TILES
;;          LOCS, a list of locations '(row col) where we are thinking about
;;                placing WORD
;;  OUTPUT: A list of tiles from WORD which are not already on the board

(defun get-new-tiles (game word locs)
  (get-new-tiles-acc game (coerce word 'list) locs nil))

(defun get-new-tiles-acc (game word locs acc)
  (cond ((null word) acc)
        ((empty-space? (scrabble-board game)
                       (first (first locs))
                       (second (first locs)))
         ;(format t "empty-space~%")
         (get-new-tiles-acc game (rest word) (rest locs)
                            (append acc (list (get-from-rack game
                                                              (first word))))))
        (t (get-new-tiles-acc game (rest word) (rest locs) acc))))

;;  PLACE-ALL-TILES!
;; -----------------------
;;  INPUTS: BOARD, a 2D array rpresenting a scrabble board.
;;          TILES, a LIST of TILEs
;;          LOCS, a LIST of LISTS of 2 integers representing the locations
;;                to place the TILEs
;;  OUTPUTS: The modified board
;;  SIDE-EFFECT: Modifies BOARD with TILEs at LOCS

(defun place-all-tiles! (game tiles locs)
  (let ((board (scrabble-board game)))
    (cond ((null tiles) board)
          (t (when (empty-space? board
                                 (first (first locs)) (second (first locs)))
               (place-tile! board
                            (first tiles)
                            (first (first locs))
                            (second (first locs)))
               (remove-from-rack! game (first tiles)))
             (place-all-tiles! game (rest tiles) (rest locs))))))

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

;;  SCORE
;; ------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board. Note that
;;                 the NEW-TILES should already be placed on this board
;;          NEW-TILES, a list of TILE structs representing tiles
;;                     on the board
;;  OUTPUT: The score for the given move

(defun score (board new-tiles)
  ;(format t "Score ~A~%" new-tiles)
  (let ((score (score-words board (get-new-words board new-tiles) new-tiles)))
    (when (= (length new-tiles) 7)
      ;; BINGO!
      (setf score (+ score 50)))
    ;(format t "Total score for ~A: ~A~%" new-tiles score)
    score))

;;  SCORE-WORDS
;; --------------------------
;;  INPUTS: BOARD, a 2D array representing a scrabble board
;;          WORDS, A LIST containing a LIST of TILEs representing words
;;  OUTPUT: The total score for all the given words

(defun score-words (board words new-tiles)
  (score-words-acc board words 0 new-tiles))

(defun score-words-acc (board words score new-tiles)
  (cond ((null words) score)
        (t (score-words-acc board
                            (rest words)
                            (+ (* (score-word board (first words) new-tiles)
                                  (get-word-multiplier (first words) new-tiles))
                               score)
                            new-tiles))))

;;  SCORE-WORD
;; ----------------------------
;;  INPUTS: BOARD, a 2D array represnting a scrabble board
;;          WORD, a LIST of TILEs representing a word
;;  OUTPUT: The score for the given word

(defun score-word (board word new-tiles)
  ;(format t "score for ~A:~A ~%" word (score-word-acc board word 0 new-tiles))
  (score-word-acc board word 0 new-tiles))

(defun score-word-acc (board word score new-tiles)
  (cond ((null word)
         ;(format t "~%")
         score)
        (t ;(format t "~A(~A*~A[~A][~A->~A]) + "
           ;        (first word)
           ;        (tile-value (first word))
           ;        (get-tile-multiplier (first word) new-tiles)
           ;        (* (get-tile-multiplier (first word) new-tiles)
           ;           (tile-value (first word)))
           ;        score
           ;        (+ score (* (get-tile-multiplier (first word) new-tiles)
           ;                    (tile-value (first word)))))
          (score-word-acc board (rest word)
                          (+ score (* (get-tile-multiplier (first word)
                                                           new-tiles)
                                      (tile-value (first word))))
                           new-tiles))))

;;  GET-TILE-MULTIPLIER
;; ------------------------
;;  INPUTS: ROW & COL, position of space on the board
;;  OUTPUT: The multiplier for that space

(defun get-tile-multiplier (tile new-tiles)
  ;; If it's not a new tile
  (when (null (member tile new-tiles :test #'equal))
    (return-from get-tile-multiplier 1))
  (let ((premium (get-space-premium (tile-row tile) (tile-col tile))))
    (cond ((equal premium *dl*) 2)
          ((equal premium *tl*) 3)
          (t 1))))

;;  GET-WORD-MULTIPLIER
;; ------------------------
;;  INPUTS: WORD, A LIST of TILEs representing a word on the board
;;  OUTPUT: The multiplier for that word. For example, a word with a tile
;;          in (0 0) would return at least 3 (possibly 9+ if it covers another
;;          triple word space.

(defun get-word-multiplier (word new-tiles)
  (get-word-multiplier-acc word new-tiles 1))

(defun get-word-multiplier-acc (word new-tiles acc)
  (if (null word)
    acc
    (let* ((row (tile-row (first word)))
           (col (tile-col (first word)))
           (premium (get-space-premium row col)))
      (cond ((and (equal premium *dw*)
                  ;; And it's a new tile
                  (not (null (member (first word) new-tiles :test #'equal))))
             (get-word-multiplier-acc (rest word)
                                      new-tiles
                                      (* acc 2)))
            ((and (equal premium *tw*)
                  ;; And it's a new tile
                  (not (null (member (first word) new-tiles :test #'equal))))
             (get-word-multiplier-acc (rest word)
                                      new-tiles
                                      (* acc 3)))
            (t (get-word-multiplier-acc (rest word) new-tiles acc))))))

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
  ;(format t "get-words-at-tile: ~A~%" tile)
  (let ((words nil)
        (horizontal (append
                      (get-word-left board (tile-row tile) (tile-col tile))
                      (list tile)
                      (get-word-right board (tile-row tile) (tile-col tile))))
        (vertical (append
                    (get-word-above board (tile-row tile) (tile-col tile))
                    (list tile)
                    (get-word-below board (tile-row tile) (tile-col tile)))))
    (when (> (length horizontal) 1)
      (setf words (append words (list horizontal))))
    (when (> (length vertical) 1)
      (setf words (append words (list vertical))))
    words))

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
  (cond ((<= n 0) game)
        ((null (scrabble-bag game)) game)
        (t (decf (scrabble-num-tiles-left game))
           (if (equal player *ply0*)
             (setf (scrabble-rack_0 game)
                   (cons (first (scrabble-bag game))
                         (scrabble-rack_0 game)))
             (setf (scrabble-rack_1 game)
                   (cons (first (scrabble-bag game))
                         (scrabble-rack_1 game))))
           (setf (scrabble-bag game) (rest (scrabble-bag game)))
           (pick-tiles! game player (1- n)))))

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
  ;(format t "---PASSING ~A~%" (whose-turn game))
  (let ((player (whose-turn game)))
    (if (equal player *ply0*)
	(setf (scrabble-whose-turn game) *ply1*)
      (setf (scrabble-whose-turn game) *ply0*))
    game))
