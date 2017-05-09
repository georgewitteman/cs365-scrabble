
;; =====================
;; Basic Definitions
;; =====================



(defparameter *files*
    (list "basic-defns"
	  "play-scr"))

;; CL
;; ----------------------
;; Compiles and Loads all files

(defun cl ()
  (dolist (file *files*)
    (compile-file file)
    (load file)))

;; DEFINE-CONSTANTS
;; ---------------------

(defconstant *dl* 'dl)
(defconstant *dw* 'dw)
(defconstant *tl* 'tl)
(defconstant *tw* 'tw)
(defconstant *open* '--)
(defconstant *ply0* 0)
(defconstant *ply1* 1)
(defconstant *blank* '-)

(defconstant *num-tiles-left* 100)



(defconstant *letters-array* 
    (make-array 27 :initial-contents
		'(- A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)))

(defconstant *initial-tiles-left* 
    (make-array 27 :initial-contents 
		;;- A B C D E  F G H I J K L M N O P Q R S T U V W X Y Z
    		'(2 9 2 2 4 12 2 3 2 9 1 1 4 2 6 8 2 1 6 4 6 4 2 2 1 2 1)))

(defconstant *letter-val-array*
    (make-array 27 :initial-contents
		'(0 1 3 3 2 1 4 2 4 1 8 5 1 3 1 1 3 10 1 1 1 1 4 4 8 4 10)))

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

;;  The TILE struct
;; -----------------------

(defstruct (tile (:print-function print-tile))
  letter
  value
  row
  col)

(defun print-tile (tile str d)
  (declare (ignore d))
  (format str "~A " (tile-letter tile)))

;;  The SCRABBLE struct
;; ---------------------

(defstruct (scrabble (:print-function print-scrabble))
  ;; BOARD: a 15x15 array of letters
  board
  ;; WHOSE-TURN
  whose-turn
  ;; RACK: a 2x15 array of letters
  rack
  ;; BAG: tiles left a 27 vector of integers
  bag
  ;; LETTER-VAL: a 27 vector of integers
  (point-values *letter-val-array*)
  ;; NUM-TILES-LEFT: integer
  num-tiles-left
  ;; SCORE: a 2 vector of integers
  score)

;; WHOSE-TURN
;; -----------------
;; INPUT: GAME, a SCRABBLE struct
;; OUTPUT: The player whose turn it is

(defun whose-turn (game)
  (scrabble-whose-turn game))

;; COPY-ARRAY
;; -----------------
;; INPUT: ARR, a 1 or 2-dimensional array
;; OUTPUT: A copy 

(defun copy-array (arr)
  (let* ((dim (array-dimensions arr))
	 (copy (make-array dim)))
    (if (= 1 (list-length dim)
	   (dotimes (x (first dim) copy)
	     (setf (svref copy x) (svref arr x))))
	(dotimes (r (first dim))
	  (dotimes (c (second dim) copy)
	    (setf (aref copy r c) (aref arr r c)))))))

;; COPY-GAME
;; -----------------
;; INPUT: GAME, a SCRABLE struct
;; OUTPUT: A copy of GAME

(defun copy-game (game)
  (make-scrabble :board (copy-array (scrabble-board game))
		 :whose-turn (scrabble-whose-turn game)
		 :rack (copy-array (scrabble-rack game))
		 :bag (copy-array (scrabble-tiles-left game))
		 :num-tiles-left (scrabble-tiles-left game)
		 :point-values (copy-array (scrabble-point-values game))
		 :score (copy-array (scrabble-score game))))

;; PRINT-SCRABBLE
;; -----------------
;; INPUT: GAME, a scrabble struct
;; SIDE EFFECT: Display SCRABBLE game

(defun print-scrabble (game str d)
  (declare (ignore d))
  (let* ((board (scrabble-board game))
	 (rack (scrabble-rack game))
	 (p (whose-turn game))
	 (point-val (scrabble-point-values game))
	 (score (scrabble-score game))
	 (bag (scrabble-bag game)))
    
    ;; Print Title
    (format str "~%                   Scrabble~%~%")
    
    
    ;; Print Board
    (dotimes (i 15)
      (dotimes (j 15)
	(format str "~A "(aref board i j)))
      (format str "~%"))
      
    
    (format str "~% ~%")
    
    ;; Print Player 1 and Player 2 Rack
    (format str "Player 1                        Player 2    ~%")
    (let ((letter 0))
      (dotimes (i 7)
	(if (equal *ply0* p)
	    (setf letter (aref rack p i))
	  (setf letter *blank*))
	(format str "~A " letter))
      (format str "                ")
      (dotimes (i 7)
	(if (equal *ply1* p)
	    (setf letter (aref rack p i))
	  (setf letter *blank*))
	(format str " ~A" letter)))
    
    (format str "~% ~% ~%")
    
    (let ((letter 0)
	  (val 0)
	  (rem 0))
      (format str "Letters: ")
      (dotimes (i 27)
	(setf letter (svref *letters-array* i)) 
	(format str "~A  " letter))
      (format str "~%Values:  ")
      (dotimes (i 27)
	(setf val (svref *letter-val-array* i))
	(format str "~A " val)
	(when (< val 10) (format str " ")))
      (format str "~%Remain:  ")
      (dotimes (i 27)
	(setf rem (svref bag i))
	(format str "~A " rem)
	(when (< rem 10) (format str " ")))
      (format str "~%"))))
     

;; NEW-SCRABBLE
;; ----------------
;; INPUTS: None
;; OUTPUT: A SCRABBLE struct representing a new game

(defun new-scrabble ()
  (let ((game (make-scrabble
	       :board *initial-board*
	       :whose-turn *ply0*
	       :bag *initial-tiles-left*
	       :num-tiles-left *num-tiles-left*
	       :rack (random-racks *initial-tiles-left* *num-tiles-left*)
	       :score (make-array 2 :initial-element 0))))
    game))


;; NTH-ELT-INDEX-ACC
;; -----------------
;; INPUT: ARR, an array of integers 
;;        N, non-negative integer
;;        I, current index
;;        ACC, accumulator 
;; OUTPUT: An index of where the nth element is

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

;; NTH-ELT-INDEX
;; -----------------
;; INPUT: ARR, N
;; OUTPUT: An index where nth element is in array 
;; (wrapper function for NTH-ELT-INDEX)

(defun nth-elt-index (arr n)
  (nth-elt-index-acc arr n 0 n))



;; RANDOM-TILE
;; -----------------
;; INPUT: BAG, an of integers, N, num tiles remaining
;; OUTPUT: A random tile
;

(defun random-tile (bag n) 
  (let* ((tile 0)
	 (rand (random n))
	 (index (nth-elt-index bag rand)))
    (setf tile (svref *letters-array* index))))


;; RANDOM-RACKS
;; -------------------
;; INPUT: BAG, an array, N, num tiles remaining
;; OUTPUT: 2x7 array of random tiles

(defun random-racks (bag n)
  (let ((racks (make-array '(2 7))))
    (dotimes (i 7 racks)
      (setf (aref racks 0 i) (random-tile bag n))
      (setf (aref racks 0 i) (random-tile bag n)))))


  
