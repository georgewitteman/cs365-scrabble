# Scrabble (CS365 Final Project)

*Emily Stamm & George Witteman*

## Files

The organization of the files is as follows. The file `start.lisp` defines some global variables and creates the `setup` function to easily compile and load all necessary files. The file `trie.lisp` defines the `TRIE` and `TR-NODE` data structures that form the word tries as used in the Appel and Jacobson algorithm. (Note that their algorithm uses a modified version of a trie called a *dawg* which uses less memory. Since we are not as concerned with memory as in 1988, we decided a trie was good enough.) The `begin.lisp` file outlines some convenient print functions. The `scrabble.lisp` file contains the implementation of the game. It sets up the `SCRABBLE` and `TILE` data structures. It also includes all the functions to make the game playable including `do-move!`, `trade-in!`, and `pass!`. The `ospd.txt` file is the scrabble dictionary we use for this implementatino, which is read using the `file-read.lisp`.

## How to Play

First compile and load the file `start.lisp`. Then you can compile and load all the other files by typing `(setup)`. Then you are ready to play the game.

### Playing Manually

Start by typing `(setf g (new-scrabble))` in the command line. This will start the Scrabble game, which will appear on the screen, in addition to some preliminary instructions. At any time, you can type `(rules)` to see the rules or `(commands)` to see the list of possible commands. To make a word, type `(do-move! g t string ‘((row[1] col[1]) ... (row[n] col[n])))` where the string is the word you want to put on the board (including the anchor letter which is already on the board) and each `(row[i] col[i])` is the row and column corresponding to the ith character in the string. For example, if you wanted to place the word "dog" as the first word on the board, you would type: 

    (do-move! g t "dog" ‘((7 7) (7 8) (7 9)))

You can also use the functions `find-best-move` to find the move with the highest score, as well as `do-best-move!` and `do-random-move!` to do a best and random move, respectively. Finally, instead of doing a move, you can trade in any number of letters by using the `trade-in!` function or pass your turn using the `pass!` function. With the `trade-in!` function, you specify the indices of the tiles in your rack that you want to trade in as a list. For instance, if you wanted to trade in the first and third tile in your rack, you would type:

    (trade-in! g ‘(0 2))
    
The following functions are also available to do the best move:

  - `(find-best-move g)`: Output the best move
  - `(do-best-move! g)`: Modifies the game `g` with the best move.
  
If you want to play against the computer you can manually do a move for yourself, and then immediately run `(do-best-move! g)` for player 2.
    
### Algorithm vs. Algorithm

You can also have the computer play games entirely on it's own. There are 3 functions to do this:

  - `(random-vs-random)`: Each player does a random move each turn
  - `(random-vs-best)`: Player 1 does a random move, and player 2 does the best scoring move (i.e. player 2 should win)
  - `(best-vs-random)`: Same as above except players are swapped
  - `(best-vs-best)`: Each player does the best scoring move
  
The files `random-vs-best.txt`, `random-vs-random.txt`, and `best-vs-best.txt` show examples of the output for these functions.

## Trie Data Structure

This algorithm makes heavy use of the *trie* structure to efficiently generate left and right-parts of words. A trie is simply a tree whose edges are labeled by letters. If the nodes from the root node to a given node create a valid word, `is-word` gets marked as `t`. A more detailed description of the *trie* is given in the Appel and Jacobson paper.

Andrew W. Appel and Guy J. Jacobson
 “The World’s Fastest Scrabble Program”
https://pdfs.semanticscholar.org/da31/cb24574f7c881a5dbf008e52aac7048c9d9c.pdf
