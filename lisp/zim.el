(defun zim-timestamp ()
  (with-temp-buffer
    (insert (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
    (forward-char -2)
    (insert ":")
    (buffer-string)))

(defun insert-zim-timestamp ()
  (interactive)
  (insert (zim-timestamp)))

(defun insert-zim-header ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert
     (concat "Content-Type: text/x-zim-wiki\n"
             "Wiki-Format: zim 0.4\n"
             "Creation-Date: " (zim-timestamp) "\n\n"))))
