(defun org-space-key (&optional arg)
  "Insert two spaces after a period."
  (interactive "p")

  (when (looking-back "[.!?…]")
    (call-interactively 'self-insert-command arg))
  (call-interactively 'self-insert-command arg))

;; From http://pages.sachachua.com/.emacs.d/Sacha.html
(defun sachachua/fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph (or REGION).
  With the prefix argument UNFILL, unfill it instead."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
    (fill-paragraph nil region)))
