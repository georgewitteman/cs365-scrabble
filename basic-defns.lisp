;; =====================
;; Basic Definitions
;; =====================

;; Useful for quickly turning off debug printing
(defconstant *debugging* t)

(defparameter *files*
  (list "basic-defns"
        "play-scrabble"
        ;"move-generation"
        "begin"
	))

;;  CL
;; ----------------------
;; Compiles and Loads all files

(defun cl ()
  (dolist (file *files*)
    (compile-file file)
    (load file)))

;;  Official Scrabble Players Dictionary
;; ---------------------------------------

(load "file-read")
(defconstant *ospd* (file-to-list "ospd.txt"))

;;  DEFINE-CONSTANTS
;; ---------------------

(defconstant *dl* 'dl)
(defconstant *dw* 'dw)
(defconstant *tl* 'tl)
(defconstant *tw* 'tw)
(defconstant *open* '--)
(defconstant *ply0* 0)
(defconstant *ply1* 1)
(defconstant *blank* '-)

(defconstant *num-tiles-left* 98)

(defconstant *letters-list*
             (list #\A #\B #\C #\D #\E
                   #\F #\G #\H #\I #\J
                   #\K #\L #\M #\N #\O
                   #\P #\Q #\R #\S #\T
                   #\U #\V #\W #\X #\Y
                   #\Z))

(defconstant *letters-array* 
             (make-array 26 :initial-contents *letters-list*))

(defconstant *initial-tiles-left-array* 
             (make-array 26 :initial-contents 
                         ;;A B C D E  F G H I J K L M N O P Q R S T U V W X Y Z
                         '(9 2 2 4 12 2 3 2 9 1 1 4 2 6 8 2 1 6 4 6 4 2 2 1 2 1)))

(defconstant *letter-val-array*
             (make-array 26 :initial-contents
                         '(1 3 3 2 1 4 2 4 1 8 5 1 3 1 1 3 10 1 1 1 1 4 4 8 4 10)))

(defconstant *initial-board*
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

;; NOTE: A WORD as referenced in these files is a LIST of TILEs

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
                 :num-tiles-left (scrabble-tiles-left game)
                 :score (copy-array (scrabble-score game))))

;;  PRINT-SCRABBLE
;; -----------------
;;  INPUT: GAME, a scrabble struct
;;  SIDE EFFECT: Display SCRABBLE game

(defun print-scrabble (game str d)
  (declare (ignore d))
  (let* ((board (scrabble-board game))
         (p (whose-turn game))
         (score (scrabble-score game))
         (bag (scrabble-bag game)))

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
            (format str "~c[43m~c[30m~2A~c[0m " #\ESC #\ESC el #\ESC)
            (format str "~3A" el))))
      (format str "~%"))
    (format str "~%")

    ;; Print Player 1 and Player 2 Rack
    (format str "Player 1~25TPlayer 2~55TBag~%")

    (format str "Rack: ")
    (dolist (tile (scrabble-rack_0 game))
      (if (equal *ply0* p)
        (print-tile tile str d)
        (format str "- ")))
    (format str "~25TRack: ")
    (dolist (tile (scrabble-rack_1 game))
      (if (equal *ply1* p)
        (print-tile tile str d)
        (format str "- ")))
    (format str "~55T~A" (scrabble-num-tiles-left game))

    (format str "~%Score: ~A~25TScore: ~A"
            (svref (scrabble-score game) 0)
            (svref (scrabble-score game) 1))

    (format str "~% ~%")

    (let ((letter 0)
          (val 0)
          (rem 0))
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
