;;; ==============================
;;; TRIE.LISP
;;; ==============================

(defconstant *ospd* (file-to-list "ospd.txt"))

;; TRIE Struct
;; ------------------------------

(defstruct trie
  (hashy (make-hash-table :test #'equal)) ; hash-table key=word, value=tr-node
  root-key)                               ; hash-table key for root node

;; TR-NODE Struct
;; ------------------------------

(defstruct tr-node
  key
  is-word
  parent
  (children (make-array 26 :initial-element nil)))

;; TR-NODE-CHAR
;; -------------------------
;; INPUTS: NODEY, a TR-NODE
;; OUTPUT: The last character in NODEY's key

(defun tr-node-char (nodey)
  (last-char (tr-node-key nodey)))

;; LAST-CHAR
;; ------------------------
;; INPUTS: STR, a STRING
;; OUTPUT: The last character in STR

(defun last-char (str)
  (char str (1- (length str))))

;; GET-ROOT-NODE
;; -------------------------------
;; INPUT: TR, a TRIE struct
;; OUTPUT: TR-NODE corresponding to root of TR

(defun get-root-node (tr)
  (gethash (trie-root-key tr) (trie-hashy tr)))

;; COPY-NODE
;; -------------------------------
;; INPUT: NODEY, a TR-NODE
;; OUTPUT: Copy of NODEY

(defun copy-node (nodey)
  (make-tr-node :key (tr-node-key nodey)
                :is-word (tr-node-is-word nodey)
                :parent (tr-node-parent nodey)
                :children (tr-node-children nodey)))


;; NEW-TRIE
;; ---------------------------------
;; INPUT: None
;; OUTPUT: New trie

(defun new-trie ()
  (let ((tr (make-trie :root-key  "")))
    (insert-node (make-tr-node :key ""
                               :is-word nil
                               :parent nil)
                 tr)
    tr))

;; INSERT-NODE
;; ----------------------------------
;; INPUT: NODEY, a TR-NODE struct, TR, a TRIE struct
;; OUTPUT: NODEY
;; SIDE EFFECT: Insert NODEY into TR

(defun insert-node (nodey tr)
  (setf (gethash (tr-node-key nodey) (trie-hashy tr)) nodey)
  nodey)

;; REMOVE-NODE
;; ------------------------------------
;; INPUT: NODEY, a TR-NODE struct, TR, a TRIE struct
;; OUTPUT: NODEY
;; SIDE EFFECT: Delete NODEY from TR

(defun delete-node (nodey tr)
  (remhash (tr-node-key nodey) (trie-hashy tr))
  nodey)

;; GET-CHILD
;; ---------------------------------
;; INPUT: NODEY, a node, INDEX, an integer 0 to 25
;; OUTPUT: Child of NODEY corresponding to INDEX

(defun get-child (nodey index tr)
  (let ((parent-key (tr-node-key nodey))
        (child-key (list (svref *letters-array* index))))
    (setf child-key (string-downcase (concatenate'string parent-key child-key)))
    (get-tr-node child-key tr)))

;; GET-CHILD-CHAR
;; -------------------------
;; INPUTS: NODEY, a TR-NODE
;;         CHR, a character
;;         TR, a TRIE
;; OUTPUT: The CHR child node from NODEY

(defun get-child-char (nodey chr tr)
  (let ((index (position chr *letters-array* :test #'char-equal)))
    (get-child nodey index tr)))

;; HAS-CHILD?
;; --------------------
;; INPUTS: NODEY, a TR-NODE
;;         CHR, a CHARACTER
;;         TR, a TRIE
;; OUTPUT: T if the node has a child CHR, NIL otherwise

(defun has-child? (nodey chr tr)
  (let ((index (position chr *letters-array* :test #'char-equal)))
    (not (null (get-child nodey index tr)))))

;; GET-CHILDREN
;; -----------------------------------
;; INPUT: NODEY, a node, TR, a TRIE struct
;; OUTPUT: List of children

(defun get-children (nodey tr)
  (let ((listy ())
        (child 0))
    (dotimes (i 26 listy)
      (when (svref (tr-node-children nodey) i)
        (setf child (get-child nodey i tr))
        (setf listy (cons child listy))))))

;; IS-WORD?
;; ------------------------------------
;; INPUT: WORD, a string, TR, TRIE struct
;; OUTPUT: T if word, NIL otherwise

(defun is-word? (word tr)
  (let ((nodey (get-tr-node word tr)))
    (if (null nodey)
      nil
      (tr-node-is-word nodey))))

;; INSERT-WORD
;; ----------------------------------
;; INPUTS: TR, a TRIE struct
;;         WORD, a string
;; OUTPUT: Newly created and inserted nodes for word
;; SIDE EFFECT: Insert new node into TR

(defun insert-word (tr word)
  (let* ((old-node (get-root-node tr))
         (new-node (get-root-node tr))
         (*str* "")              ; string that accumulates word
         (*char* (char word 0))  ; current character in word
         (index 0)               ; index in letter array
         (stay-case-1 nil)  ; boolean tells us if we've hit CASE 1 once
         (len (length word))     ; length of the word
         )

    (dotimes (i len new-node)

      (setf *char* (char word i))
      (setf index (position *char* *letters-array* :test #'char-equal))
      (setf *str* (concatenate 'string *str* (list *char*)))

      (cond
        ;; Case 1: children array of OLD-NODE does NOT contain next character
        ((or stay-case-1 
             (null (svref (tr-node-children old-node) index)))

         ;; Create child node
         (if (equal word *str*)
           (setf new-node (make-tr-node :key word :is-word t :parent old-node))
           (setf new-node (make-tr-node :key *str* :is-word nil :parent old-node)))
         ;; Insert NEW-NODE into TR
         (insert-node new-node tr)
         ;; Delete OLD-NODE from TR
         (delete-node old-node tr)
         ;; Update children array of OLD-NODE
         (setf (svref (tr-node-children old-node) index) t)
         ;; Insert OLD-NODE back into Tr
         (insert-node old-node tr)
         ;; Set OLD-NODE to current NEW-NODE
         (setf old-node (copy-node new-node))
         ;; Set STAY-CASE-1 to T
         (setf stay-case-1 t))

        ;; Case 2: children array of OLD-NODE contains next character
        (T
          ;; Set OLD-NODE to its child
          (setf old-node (get-child old-node index tr)))))))

;; GET-TR-NODE
;; -------------------------------
;; INPUT: WORD, a string, TR, a TRIE struct
;; OUTPUT: Corresponding node

(defun get-tr-node (word tr)
  (gethash (string-downcase word) (trie-hashy tr)))

;; ADD-ALL-WORDS
;; ------------------------------
;; INPUT: TR, a TRIE struct
;; OUTPUT: TR modified
;; SIDE EFFECT: Add all words to TR

(defun add-all-words (tr)
  (dolist (word *ospd* tr)
    (insert-word tr word)))
