(defparameter *dl* 'dl)
(defparameter *dw* 'dw)
(defparameter *tl* 'tl)
(defparameter *tw* 'tw)
(defparameter *open* '--)
(defparameter *ply0* 0)
(defparameter *ply1* 1)
(defparameter *blank* '-)
(defparameter *num-tiles-left* 98)

(defparameter *initial-tiles-left-array*
  (make-array 26 :initial-contents
              ;;A B C D E  F G H I J K L M N O P Q R S T U V W X Y Z
              '(9 2 2 4 12 2 3 2 9 1 1 4 2 6 8 2 1 6 4 6 4 2 2 1 2 1)))

(defparameter *letter-val-array*
  (make-array 26 :initial-contents
              '(1 3 3 2 1 4 2 4 1 8 5 1 3 1 1 3 10 1 1 1 1 4 4 8 4 10)))

(defparameter *initial-board*
  (make-array '(15 15) :initial-contents
              '((tw -- -- dl -- -- -- tw -- -- -- dl -- -- tw)
                (-- dw -- -- -- tl -- -- -- tl -- -- -- dw --)
                (-- -- dw -- -- -- dl -- dl -- -- -- dw -- --)
                (dl -- -- dw -- -- -- dl -- -- -- dw -- -- dl)
                (-- -- -- -- dw -- -- -- -- -- dw -- -- -- --)
                (-- tl -- -- -- tl -- -- -- tl -- -- -- tl --)
                (-- -- dl -- -- -- dl -- dl -- -- -- dl -- --)
                (tw -- -- dl -- -- -- dw -- -- -- dl -- -- tw)
                (-- -- dl -- -- -- dl -- dl -- -- -- dl -- --)
                (-- tl -- -- -- tl -- -- -- tl -- -- -- tl --)
                (-- -- -- -- dw -- -- -- -- -- dw -- -- -- --)
                (dl -- -- dw -- -- -- dl -- -- -- dw -- -- dl)
                (-- -- dw -- -- -- dl -- dl -- -- -- dw -- --)
                (-- dw -- -- -- tl -- -- -- tl -- -- -- dw --)
                (tw -- -- dl -- -- -- tw -- -- -- dl -- -- tw))))

(defparameter *legal-moves* nil)

(defparameter *trie* (new-trie))
(add-all-words *trie*)

;;  The MOVE struct
;; ----------------------

(defstruct (move)
  tiles      ; A list of TILE structs
  locations  ; A list of locations ('(row col))
  )

;;  The TILE struct
;; -----------------------

(defstruct (tile (:print-function print-tile))
  letter ; CHARACTER, the letter on the tile (- for blank)
  value  ; integer value of the tile
  row    ; position of the tile, NIL if not yet on the board
  col    ; "
  )

;;  The SCRABBLE struct
;; ---------------------

(defstruct (scrabble (:print-function print-scrabble))
  board          ; a 15x15 array
  whose-turn     ; *ply0* or *ply1*
  rack_0         ; a LIST of TILEs
  rack_1         ; "
  bag            ; a LIST of TILEs representing the tiles left in the bag
  num-tiles-left ; number of tiles left in the bag
  score          ; a vector containing each players score
  )


;; PRINT-TILE
;; -------------------------

(defun print-tile (tile str d)
  (declare (ignore d))
  (format str "~A|" (tile-letter tile)))

;; TILE-EQ?
;; -------------------------

(defun tile-eq? (tile-1 tile-2)
  (char-equal (tile-letter tile-1)
              (tile-letter tile-2)))

;;  WORD-TO-STRING
;; -------------------------
;;  INPUTS: WORD, a LIST of TILEs
;;  OUTPUTS: A string that corresponds to WORD

(defun word-to-string (word)
  (let ((list-o-chars nil))
    (dolist (tile word)
      (setf list-o-chars (append list-o-chars (list (tile-letter tile)))))
    (coerce list-o-chars 'string)))

;;  MAKE-BAG
;; -----------------------
;;  INPUT: None
;;  OUTPUT: List of initial tile

(defun make-bag ()
  (let ((bag '()))
    (dotimes (i 26 bag)
      (dotimes (j (svref *initial-tiles-left-array* i))
        (setf bag (cons (make-tile :letter (svref *letters-array* i)
                                   :value (svref *letter-val-array* i))
                        bag))))))

;;  WHOSE-TURN
;; -----------------
;;  INPUT: GAME, a SCRABBLE struct
;;  OUTPUT: The player whose turn it is

(defun whose-turn (game)
  (scrabble-whose-turn game))

;;  COPY-ARRAY
;; -----------------
;;  INPUT: ARR, an ARRAY
;;  OUTPUT: A copy

(defun copy-array (arr)
  (let ((dims (array-dimensions arr)))
    (adjust-array
      (make-array dims :displaced-to arr)
      dims)))

;;  COPY-GAME
;; -----------------
;;  INPUT: GAME, a SCRABLE struct
;;  OUTPUT: A copy of GAME

(defun copy-game (game)
  (make-scrabble :board (copy-array (scrabble-board game))
                 :whose-turn (scrabble-whose-turn game)
                 :rack_0 (copy-list (scrabble-rack_0 game))
                 :rack_1 (copy-list (scrabble-rack_1 game))
                 :bag (copy-list (scrabble-bag game))
                 :num-tiles-left (scrabble-num-tiles-left game)
                 :score (copy-seq (scrabble-score game))))

;;  PRINT-SCRABBLE
;; -----------------
;;  INPUT: GAME, a scrabble struct
;;  SIDE EFFECT: Display SCRABBLE game

(defun print-scrabble (game str d)
  (let* ((board (scrabble-board game))
         (p (whose-turn game)))

    ;; Print Title
    (format t "~4T   ____________  ___   ___  ___  __   ____~%")
    (format t "~4T  / __/ ___/ _ \\/ _ | / _ )/ _ )/ /  / __/~%")
    (format t "~4T _\\ \\/ /__/ , _/ __ |/ _  / _  / /__/ _/~%")
    (format t "~4T/___/\\___/_/|_/_/ |_/____/____/____/___/~%")
    (format t "~%")

    ;; Print Board
    (format t "~4T0  1  2  3  4  5  6  7  8  9  10 11 12 13 14~%")
    (dotimes (i 15)
      (format t "~2@A: " i)
      (dotimes (j 15)
        (let ((el (aref board i j)))
          (if (tile-p el)
            ;; Print tiles on the board with a yellow bg and black text
            (format str "~2A " el)
            ;(format str "~c[43m~c[30m~2A~c[0m " #\ESC #\ESC el #\ESC)
            (format str "~3A" el))))
      (format str "~%"))
    (format str "~%")

    ;; Print Player 1 and Player 2 Rack
    (format str "Player 1~25TPlayer 2~55TBag~%")

    (format str "Rack: ")
    (dolist (tile (scrabble-rack_0 game))
      ;(format str "~A" tile))
      (if (equal *ply0* p)
        (print-tile tile str d)
        (format str "- ")))
    (format str "~25TRack: ")
    (dolist (tile (scrabble-rack_1 game))
      ;(format str "~A" tile))
      (if (equal *ply1* p)
        (print-tile tile str d)
        (format str "- ")))
    (format str "~55T~A" (scrabble-num-tiles-left game))

    (format str "~%Score: ~A~25TScore: ~A"
            (svref (scrabble-score game) 0)
            (svref (scrabble-score game) 1))

    (format str "~% ~%")

    (let ((letter 0)
          (val 0))
      (format str "Letters: ")
      (dotimes (i 26)
        (setf letter (svref *letters-array* i))
        (format str "~2A" letter))
      (format str "~%Values:  ")
      (dotimes (i 26)
        (setf val (svref *letter-val-array* i))
        (format str "~2A" val))
      (format str "~%"))))

;;  NEW-SCRABBLE
;; ----------------
;;  INPUTS: None
;;  OUTPUT: A SCRABBLE struct representing a new game

(defun new-scrabble ()
  (begin-scrabble)
  (let ((game (make-scrabble
                :board (copy-array *initial-board*)
                :whose-turn *ply0*
                :bag (make-bag)
                :num-tiles-left *num-tiles-left*
                :rack_0 ()
                :rack_1 ()
                :score (make-array 2 :initial-element 0))))
    (shake-bag! game)
    (pick-tiles! game *ply0* 7)
    (pick-tiles! game *ply1* 7)
    game))


;;  NTH-ELT-INDEX-ACC
;; -----------------
;;  INPUT: ARR, an array of integers
;;         N, non-negative integer
;;         I, current index
;;         ACC, accumulator
;;  OUTPUT: An index of where the nth element is

(defun nth-elt-index-acc (arr n i acc)
  (cond
    ;; BASE CASE 1: N = 0
    ((= n 0)
     i)
    ;; BASE CASE 2: ACC = 0
    ((<= acc 0)
     (decf i))
    ;; RECURSIVE CASE: ACC < N
    (T
      (nth-elt-index-acc arr n (incf i) (- acc (svref arr (- i 1)))))))

;;  NTH-ELT-INDEX
;; -----------------
;;  INPUT: ARR, N
;;  OUTPUT: An index where nth element is in array
;;  (wrapper function for NTH-ELT-INDEX)

(defun nth-elt-index (arr n)
  (nth-elt-index-acc arr n 0 n))

;;  SHAKE-BAG!
;; ------------------
;;  INPUT: GAME, a scrabble struct
;;  OUTPUT: GAME, where the bag has been randomized

(defun shake-bag! (game)
  (setf (scrabble-bag game)
        (shake-bag-acc (scrabble-bag game) ())))

(defun shake-bag-acc (bag new-bag)
  (cond
    ;; BASE CASE: BAG is empty
    ((null bag)
     new-bag)
    ;; RECURSIVE CASE: BAG is nonempty
    (T
      (let* ((length (list-length bag))
             (rand (random length))
             (tile (nth rand bag)))
        (shake-bag-acc (remove tile bag :count 1)
                       (cons tile new-bag))))))
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
  (let ((new-tiles (get-new-tiles game word locs))
        (new-locs (get-new-locs game locs)))
    (when (or (not check-legal?)
              (is-legal? game word locs))
      (place-all-tiles! game new-tiles new-locs)
      (refill-racks! game)
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
  (let ((score (score-words board (get-new-words board new-tiles) new-tiles)))
    (when (= (length new-tiles) 7)
      ;; BINGO!
      (setf score (+ score 50)))
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
         score)
        (t
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

;; CLEAR-LEGAL-MOVES
;; -------------------------
;; INPUTS: ---
;; OUTPUT: ---
;; SIDE-EFFECT: Sets *LEGAL-MOVES* to NIL

(defun clear-legal-moves ()
  (setf *legal-moves* nil))

;; ADD-LEGAL-MOVE
;; -----------------------
;; INPUTS: TILES, a list of TILEs
;;         LOCS, a list of locations
;;         TRANSPOSED, t if the board is tranposed
;; OUTPUT: ---
;; SIDE-EFFECT: Adds the move to *LEGAL-MOVES*

(defun add-legal-move! (tiles locs transposed)
  (setf tiles (word-to-string tiles))
  (setf *legal-moves* (cons (list tiles (if transposed
                                          (transpose-locs locs)
                                          locs)) *legal-moves*)))

;; TRANSPOSE-LOCS
;; --------------------------------
;; INPUTS: LOCS, a list of locations
;; OUTPUT: The list of locations where each location has it's row/col swapped

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
    (left-part game nil nil '(7 7) (get-root-node *trie*)
               (get-limit (scrabble-board game) 7 7) nil)
    (transpose-board! game)
    (left-part game nil nil '(7 7) (get-root-node *trie*)
               (get-limit (scrabble-board game) 7 7) t)
    (transpose-board! game)
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
;;          PARTIAL-LOCS
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
               (add-legal-move! (append prefix partial-word) partial-locs
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
  (let* ((moves (generate-moves game))
         (best-move (first moves))
         (best-score 0))
    (dolist (move moves)
      (let* ((game-copy (copy-game game))
             (word (first move))
             (locs (second move))
             (score (do-move! game-copy t word locs)))
        (when (> score best-score)
          (setf best-move move)
          (setf best-score score))))
    best-move))

;; BEST-VS-BEST
;; -------------------------

(defun best-vs-best ()
  (let ((gm (new-scrabble)))
    (while (not (game-over? gm))
           (format t "~A~%" gm)
           (do-best-move! gm))
    (format t "~A~%" gm)
    (if (> (svref (scrabble-score gm) 0) (svref (scrabble-score gm) 1))
      (format t "~%Player 1 WINS!~%")
      (format t "~%Player 2 WINS!~%"))))

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

(defun best-vs-random ()
  (let ((gm (new-scrabble)))
    (while (not (game-over? gm))
           (format t "~A~%" gm)
           (if (= (whose-turn gm) 0)
             (do-best-move! gm)
             (do-random-move! gm)))
    (format t "~A~%" gm)
    (if (> (svref (scrabble-score gm) 0) (svref (scrabble-score gm) 1))
      (format t "~%Player 1 WINS!~%")
      (format t "~%Player 2 WINS!~%"))))

;;  RANDOM-VS-RANDOM
;; -------------------
;;  INPUTS: NONE!
;;  OUTPUT: NIL
;;  SIDE-EFFECT: Creates and plays a random game

(defun random-vs-random ()
  (let ((gm (new-scrabble)))
    (while (not (game-over? gm))
           (format t "~A~%" gm)
           (do-random-move! gm))
    (format t "~A~%" gm)
    (if (> (svref (scrabble-score gm) 0) (svref (scrabble-score gm) 1))
      (format t "~%Player 1 WINS!~%")
      (format t "~%Player 2 WINS!~%"))))

;; DO-BEST-MOVE!
;; ------------------------
;; INPUT: GAME, a Scrabble struct
;; OUTPUT: GAME modified so that best move is played

(defun do-best-move! (game)
  (let ((movie (find-best-move game)))
    (format t "~%Best Move: ~A @ ~A~%" (first movie) (first (second movie)))
    (do-move! game nil (first movie) (second movie))))


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
    (format t "~%Random Move: ~A @ ~A~%" word (first locs))
    (if (null word)
      (pass! game)
      (do-move! game t word locs)))
  game)

