;;; ==============================
;;; TRIE.LISP
;;; ==============================

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
			       :is-word t
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
    (tr-node-is-word nodey)))

       
       
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
	 (stay-case-1 nil)	 ; boolean tells us if we've hit CASE 1 once
	 (len (length word))     ; length of the word
	 (chil-array (make-array 26)))
	 
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
  (gethash word (trie-hashy tr)))



;; ADD-ALL-WORDS
;; ------------------------------
;; INPUT: TR, a TRIE struct
;; OUTPUT: TR modified
;; SIDE EFFECT: Add all words to TR

(defun add-all-words (tr)
  (dolist (word *ospd* tr)
    (insert-word tr word)))

;; PRINT-OSPD-NODES
;; ---------------------------
;; INPUT: TR, a TRIE struct
;; OUTPUT: None
;; SIDE EFFECT: Prints all nodes

(defun print-ospd-nodes (tr)
  (dolist (word *ospd*)
    (format t "Key: ~A,     IS-WORD?: ~A,      PARENT-KEY: ~A ~%" word 
	    (tr-node-is-word (get-tr-node word tr)) 
	    (tr-node-key (tr-node-parent (get-tr-node word tr))))))
  
  