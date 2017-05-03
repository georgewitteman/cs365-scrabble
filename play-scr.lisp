;; ========================
;; PLAY-SCR.LISP
;; ========================

;; DO-MOVE! 
;; ------------------------


;; SCORE
;; ------------------------
;; INPUT: GAME, a SCRABBLE struct
;; SIDE EFFECT: A number of points for a given turn



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
;; SIDE EFFECT: Remove letter from rack

(defun remove-from-rack! (game letter)
  )

;; PICK-TILES!
;; ------------------------

(defun pick-tiles! (game n)
  (let ((ply (whose-turn game))
	(bag (scrabble-bag game))
	(num (scrabble-num-tiles-left game))
	(letter 'A))
 
    (dotimes (i 7)
      ;; When there is not a tile (represented by 0)
      (when (not (= (aref racks ply i) 0))
	;; Set LETTER to a random-tile from the bag
	(setf letter (random-tile bag num)) 
	;; Replace 0 with the LETTER
	(setf (aref (scrabble-rack game) ply i) letter)
	;; Remove LETTER from the bag 
	))))
	
