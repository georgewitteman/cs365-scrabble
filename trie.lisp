;;; ==============================
;;; TRIE.LISP
;;; ==============================

;; TRIE Struct
;; ------------------------------

(defparameter *dicitonary* "ospd.txt")

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
  (make-trie :root-key  ""))


;; INSERT-NODE
;; ----------------------------------
;; INPUT: NODEY, a TR-NODE struct, TR, a TRIE struct
;; OUTPUT: NODEY
;; SIDE EFFECT: Insert NODEY into TR

(defun insert-node (nodey tr)
  (setf (gethash (tr-node-key nodey) (trie-hashy tr)) nodey)
  nodey)

;; GET-CHILD
;; ---------------------------------
;; INPUT: NODEY, a node, INDEX, an integer 0 to 25
;; OUTPUT: Child of NODEY corresponding to INDEX

(defun get-child (nodey index)
  (let ((parent-key (tr-node-key nodey))
	(child-key (list (svref *letters-array* index))))
    (setf child-key (concatenate'string parent-key child-key)))) 

;; INSERT-WORD
;; ----------------------------------
;; INPUTS: TR, a TRIE struct
;;         WORD, a string
;; OUTPUT: Newly created and inserted nodes for word
;; SIDE EFFECT: Insert new node into TR

(defun insert-word (tr word)
  (let* ((old-node (get-root-node tr))
	 (new-node (make-tr-node
		:key word
		:is-word T
		:parent nil))
	 (*str* "")
	 (*char* (char word 0))
	 (index -1)
	 (i 0))
	     
    (loop while (not (equal *str* word))
		
		(setf *char* (char *str* i))
		(setf index (position *char* *letters-array* :test char-equal))
				
		;; CASE 1: If children array of OLD-NODE does NOT contain next character
		(cond
		 ((not (svref (tr-node-children old-node) index))
		  ;; Set that child index in array to T in OLD-NODE
		  (setf (svref (tr-node-children (get-hash (tr-node-key old-node) (trie-hashy tr))) index) T)
		  
		  (loop while (not (equal *str* word))
			      ;; Add character to *STR*
			      (setf *str* (concatenate 'string *str* (list *char*)))
			      ;; Create a new node
			      (setf new-node (make-tr-node 
					      :key *str*
					      :is-word nil
					      :parent old-node))
			      (when (equal *str* word)
				(setf (tr-node-is-word new-node) T))
			      ;; Insert new node into tree
			      (insert-node new-node tr)
			      ;; Increment I
			      (incf i)
			      ;; Reset *CHAR*
			      (setf *char* (char *str* i))
			      ;; Reset INDEX
			      (setf index (position *char* *letters-array* :test char-equal))
			      ;; Reset old-node
			      (setf old-node (copy-node new-node)))
		  
		  (return-from insert-word new-node))
		
		;; CASE 2: Else if children array contains next character letter
		 (T
		  ;; Set OLD-NODE equal to that child
		  (setf (old-node (get-child old-node index)))
		  ;; Increment I
		  (incf i)
		  ;; Add *char* to *str*
		  (setf *str* (concatenate 'string *str* (list *char*))))))))
		
      
;; ADD-ALL-WORDS
;; ------------------------------
;; INPUT: TR, a TRIE struct, DICTIONARY, list of strings
;; OUTPUT: TR modified
;; SIDE EFFECT: Add all words to TR

(defun add-all-words (tr dictionary)
  
  