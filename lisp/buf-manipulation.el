;; Some custom functions for buffer content manipulation

(defun delete-current-line ()
  "Kill the whole line on which point is"
  (interactive)

  (beginning-of-line)
  (kill-line 1))

(defun copy-func-prototype ()
  "Copy the current function's prototype to the kill-ring"

  (interactive)

  (save-excursion
    (beginning-of-defun)
    (setq protocopy-begin (point))
    (forward-list)
    (setq protocopy-end (point))
    (kill-ring-save protocopy-begin protocopy-end)))

(defun duplicate-line()
  "Duplicate line at point."

  (interactive)

  (save-excursion
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank)))

(defun toggle-char-case (arg-move-point)
  "Toggle the case of the char after point. Based on Xah's toggle letter
case defun version 2015-12-22

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2016-02-16"
  (interactive "P")
  (let ((case-fold-search nil))
    (cond
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))
    (cond
     (arg-move-point (right-char)))))

; Copied from http://whattheemacsd.com/editing-defuns.el-01.html
(defun open-line-below ()
  "Open a new line below point."

  (interactive)

  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  "Open a new line above point."

  (interactive)

  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

; Copied from http://whattheemacsd.com/file-defuns.el-01.html
(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)

  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          ; TODO: this is suspicious for me…
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

; Copied from http://whattheemacsd.com/file-defuns.el-02.html
(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills the
  buffer."
  (interactive)

  (let ((filename (buffer-file-name))
        (name (buffer-name))
        (buffer (current-buffer)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

; delete-char or close eshell
; Copied from https://ryuslash.org/posts/C-d-to-close-eshell.html
(defun eshell-C-d ()
  "Either call `delete-char' interactively or quit."
  (interactive)

  (condition-case err
      (call-interactively #'delete-char)
    (error (if (and (eq (car err) 'end-of-buffer)
                    (looking-back eshell-prompt-regexp))
               (kill-buffer)
             (signal (car err) (cdr err))))))

(defun æ-kill-or-copy-whole-line (kill)
  "Kill or copy the whole line point is on.

If KILL is non-nil, the line gets killed. Otherwise, it gets just
copied to the kill-ring."
  (interactive "P")

  (if kill
      (kill-whole-line)
    (let ((beginning (progn (beginning-of-line) (point)))
          (end (progn (end-of-line) (point))))
      (copy-region-as-kill beginning end))))
