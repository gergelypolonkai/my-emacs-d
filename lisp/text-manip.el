(defun org-space-key (&optional arg)
  "Insert two spaces after a period."
  (interactive "p")

  (when (looking-back "[.!?…]")
    (call-interactively 'self-insert-command arg))
  (call-interactively 'self-insert-command arg))
