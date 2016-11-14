(defun open-this-file-as-other-user (user)
  "Edit current file as USER, using `tramp' and `sudo'.  If the current
buffer is not visiting a file, prompt for a file name."
  (interactive "sEdit as user (default: root): ")
  (when (string= "" user)
    (setq user "root"))
  (let* ((filename (or buffer-file-name
                       (read-file-name (format "Find file (as %s): "
                                               user))))
         (tramp-path (concat (format "/sudo:%s@localhost:" user) filename)))
    (if buffer-file-name
        (find-alternate-file tramp-path)
      (find-file tramp-path))))

(defun gpolonkai/visit-init-file ()
  "Open the init file."
  (interactive)
  (find-file-other-window user-init-file))

(defun gpolonkai/visit-org-index ()
  "Visit the root of Org-mode notes."
  (interactive)
  (find-file-other-window (concat (file-name-as-directory org-directory)
                                  "index.org")))
