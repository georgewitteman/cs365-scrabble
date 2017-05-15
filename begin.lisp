;;; ===================
;;; BEGIN.lisp
;;; ==================


(defun begin-scrabble ()
 (format t "~%~%
  ==========================================================================
  Welcome to Scrabble! 
  ==========================================================================
  To begin, set (new-scrabble) equal to variable, ie (setf g (new-scrabble))
  To read the rules, use the RULES function, ie type (rules)
  To see a list of functions, use the COMMANDS function, ie type (commands) 
  --------------------------------------------------------------------------~%~%"))

(defun rules ()
 (format t "~%~%
  --------------------------------------------------------------------
  RULES
  --------------------------------------------------------------------~%~%")
 (format t 
  "The goal of Scrabble is to score as many points as possible by placing tiles
  on the board to create words. Each player has a rack of 7 tiles. Players take 
  turns moving, where for each move a player can either build a word (DO-MOVE!),
  pass their turn (PASS!), or trade in some amount of their tiles (TRADE-IN!). 
  For information on these functions, use the COMMANDS function. GENERATE-MOVES 
  shows all possible words you can build. The game ends when either one player 
  of tiles or neither player can build a word.

  Note: To simplify this game, we make it a 2-Player game and remove blank tiles.
  ----------------------------------------------------------------------------------~% ~%"))


(defun commands ()
  (format t "~%~%
-----------------------------------------------------------------------
COMMANDS
-----------------------------------------------------------------------~%~%")
  (format t "NEW-SCRABBLE ~%INPUTS: none. ~%OUTPUT: A new SCRABBLE struct~%~%")
  (format t "IS-A-WORD? 
INPUT: WORD, a string
OUTPUT: T if WORD is a word, NIL otherwise~%~%")
  (format t "GENERATE-MOVES~%INPUT: GAME.
OUTPUT: List of all possible moves a player can make at a given turn.~%~%")
  (format t "DO-MOVE!~%INPUTS: GAME, a SCRABBLE struct, 
 CHECK-LEGAL? T or NIL,
 WORD, a string representing letters in the word you want to play 
       (including letters on the board in direction of main word),
 LOCS, list of locations  ((r_1 c_1) (r_2 c_2) ... (r_n c_n)) 
       corresponding chars in WORD. 
OUTPUT: Modfied GAME with move done.~%~%")
  (format t "TRADE-IN!
INPUTS: GAME, a scrabble struct, 
 LISTY, a list corresponding to indices of tiles in rack that you want to trade
OUTPUT: Modified GAME so tiles specified have been traded in.~%~%")
  (format t "PASS!
INPUT: GAME, a SCRABBLE struct. 
OUTPUT: GAME modified so that it is the other player's turn.~%~%")
  (format t "FIND-BEST-MOVE
INPUT: GAME, a SCRABBLE struct.
OUTPUT: The move with the highest score.~%~%")
  (format t "DO-BEST-MOVE! 
INPUT: GAME, a SCRABBLE struct
OUTPUT: GAME modified such that the current player has done the best move.~%~%")
  (format t "DO-RANDOM-MOVE!
INPUT: GAME, a SRABBLE struct.
OUTPUT: GAME modified such that the current player has done a random move.~%")
  (format t "
----------------------------------------------------------------------~%~%"))
