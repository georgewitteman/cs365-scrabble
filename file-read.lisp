;; FILE-TO-ARRAY
;; ---------------------------
;; INPUTS: FILENAME, the name of a file as a STRING
;; OUTPUT: An array where each line of the array is an element in FILENAME

(defun file-to-array (filename)
  (let ((file-as-list (file-to-list filename)))
    (make-array (length file-as-list)
                :initial-contents file-as-list)))

;; FILE-TO-LIST
;; ---------------------------
;; INPUTS: FILENAME, the name of a file as a STRING
;; OUTPUT: A list where each element is a line in FILENAME

(defun file-to-list (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))
