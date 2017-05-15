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
  ----------------------------------------------------------------------------------
  RULES
  ----------------------------------------------------------------------------------~%~%")
 (format t 
  "The goal of Scrabble is to score as many points as possible by placing tiles on
  the board to create words. Each player has a rack of 7 tiles. Players take turns 
  moving, where for each move a player can either build a word (DO-MOVE!),
  pass their turn (PASS!), or trade in some amount of their tiles (TRADE-IN!). 
  For information on these functions, use the COMMANDS function. GENERATE-MOVES 
  shows all possible words you can build. The game ends when either one player runs out
  of tiles or neither player can build a word.

  Note: To simplify this game, we make it a 2-Player game and remove blank tiles.
  ----------------------------------------------------------------------------------~% ~%"))


(defun commands ()
  (format t "~%~%
-------------------------------------------------------------------------------------------------------
COMMANDS
-------------------------------------------------------------------------------------------------------~%~%")
  (format t "NEW-SCRABBLE: Creates a new SCRABBLE struct. ~%INPUTS: none. ~%OUTPUT: A new SCRABBLE struct~%~%")
  (format t "IS-A-WORD?: 
INPUT: WORD, a string
OUTPUT: T if WORD is a word, NIL otherwise~%~%")
  (format t "GENERATE-MOVES: Generates all possible words a player can make at a given turn.~%INPUT: GAME. ~%OUTPUT: List of moves.~%~%")
  (format t "DO-MOVE!: Builds a word.~%INPUTS: GAME, a SCRABBLE struct, 
 CHECK-LEGAL? T or NIL,
 WORD, a string representing letters in the word you want to play (including letters on the board in direction of main word),
 LOCS, a list of locations of the form ((r_1 c_1) (r_2 c_2) ... (r_n c_n)) corresponding to the characters in WORD. ~%OUTPUT: Modfied GAME with move done.~%~%")
  (format t "TRADE-IN!: Trade in letters. 
INPUTS: GAME, a scrabble struct, N, number of tiles you want to trade in.
OUTPUT: Modified GAME such that letters have been traded in and it is now the other player's turn.~%~%")
  (format t "PASS!: Pass your turn. 
INPUT: GAME, a SCRABBLE struct. 
OUTPUT: GAME modified so that it is the other player's turn.~%")
  (format t "
--------------------------------------------------------------------------------------------------------~~%~%"))
