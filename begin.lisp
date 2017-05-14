;;; ===================
;;; BEGIN.lisp
;;; ==================


(defun new-scrabble-game (game)
  (format t "Welcome to Scrabble! ~%")
  (format t "-------------------- ~%" )
  (setf game (new-scrabble))
  
  
  )



(defun rules ()
  (format t "~%     Rules      ~%")
  (format t "--------------- ~%")
  (format t 
"The goal of Scrabble is to score as many points as possible by placing tiles on
the board to create words. Each player has a rack of 7 tiles. Players take turns 
take turns moving, where for each move a player can either build a word, pass their
turn, or trade in some amount of their tiles. GENERATE-MOVES takes in the game and 
shows all possible words you can build. The game ends when either one player runs out
of tiles or neither player can build a word.

To view all commands, use the function COMMANDS, which does not take in any inputs.

Note: To simplify this game, we make it a 2-Player game and remove blank tiles. ~%"))


(defun commands ()
  (format t "Commands ~%")
  (format t "---------~%")
  (format t "NEW-SCRABBLE: ~%")
  (format t "DO-MOVE: ~%"))