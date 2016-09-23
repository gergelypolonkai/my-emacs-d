(defun æ-enclose-region (character &optional start end)
  "Enclose region in CHARACTER. If region is empty, simply inserts
  CHARACTER two times and moves point between them.

  If character is present in `insert-pair-alist', this function
  will enclose region in the corresponding pair. In this case,
  CHARACTER must be the opening member of the pair."

  (interactive "cWhat character? \nr")

  (setq open character close character)

  (let ((pair (assq character insert-pair-alist)))
    (if pair
        (if (nth 2 pair)
            (setq open (nth 1 pair) close (nth 2 pair))
          (setq open (nth 0 pair) close (nth 1 pair)))))

  (unless (and open close)
    (setq open character)
    (setq close character))

  (unless (use-region-p)
    (setq start (point) end (point)))

  (save-excursion
    (goto-char end)
    (insert-char close)

    (goto-char start)
    (insert-char open))

  (unless (use-region-p)
      (forward-char)))
