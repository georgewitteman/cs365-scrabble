;; =====================
;; START.LISP
;; Define's constants and convenience functions
;; =====================

;; Useful for quickly turning off debug printing
(defparameter *debugging* t)

;; List of all letters in the alphabet as CHARACTERS
(defparameter *letters-list*
              (list #\A #\B #\C #\D #\E
                   #\F #\G #\H #\I #\J
                   #\K #\L #\M #\N #\O
                   #\P #\Q #\R #\S #\T
                   #\U #\V #\W #\X #\Y
                   #\Z))

;; An array made up from *LETTERS-LIST*
(defparameter *letters-array*
              (make-array 26 :initial-contents *letters-list*))

(defparameter *files*
  (list
    "file-read"
    "trie"
    "begin"
    "scrabble"
    ))

(defun setup ()
  (dolist (file *files*)
    (compile-file file)
    (load file)))
