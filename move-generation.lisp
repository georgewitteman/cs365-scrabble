;;  MOVE-GENERATION.LISP
;; =====================

(defparameter *legal-moves* nil)

(defparameter *trie* (new-trie))
(add-all-words *trie*)

(defun clear-legal-moves ()
  (setf *legal-moves* nil))

(defun add-legal-move (tiles locs)
  (format t "A valid move:~%")
  (setf tiles (word-to-string tiles))
  (format t "~6T~A~%" tiles)
  (format t "~6T~A~%" locs)
  (setq *legal-moves* (cons (list tiles locs) *legal-moves*)))

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

(defun is-word-chars (word)
  (let ((word-str ""))
    (dolist (til word)
      (setf word-str (concatenate 'string word-str (string til))))
    (if (null (position word-str *ospd* :test #'word-equal?))
      nil
      t)))

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
  (let ((board (scrabble-board game)))
    ;; For each anchor
    (dolist (anchor (find-anchors board))
      (format t "Generating moves for anchor ~A~%" anchor)
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
                          (list row col)))
          ;; Create all possible left parts
          (left-part game nil nil anchor (get-root-node *trie*)
                     (get-limit board row col)))))))

(defun get-locs-from-word (word)
  (map 'list #'(lambda (tile) (list (tile-row tile) (tile-col tile))) word))

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

(defun left-part (game partial-word partial-locs anchor nodey limit)
  (extend-right game partial-word nil
                (append partial-locs (list 'L)) nodey anchor)
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
                     anchor child (1- limit))
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

(defun extend-right (game prefix partial-word partial-locs nodey square)
  ;(format t "Checking square: ~A~%" square)
  (if (empty-space? (scrabble-board game) (first square) (second square))
    ;; if SQUARE is vacant then
    (progn (when (and (tr-node-is-word nodey)
                      (not (null partial-word)))
             ;; if NODEY is a terminal node then
             ;; LegalMove(PartialWord)
             (add-legal-move (append prefix partial-word) partial-locs))
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
                                 next-square)
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
                          next-square)))))))

             

























