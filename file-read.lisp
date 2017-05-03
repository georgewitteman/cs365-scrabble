(defun file-to-array (filename)
  (let ((file-as-list (file-to-list filename)))
    (make-array (length file-as-list)
                :initial-contents file-as-list)))

(defun file-to-list (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))
