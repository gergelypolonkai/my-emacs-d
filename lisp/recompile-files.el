(defun recompile-stale-elcs ()
  (interactive)

  (with-temp-buffer
    (setq-local default-directory user-emacs-directory)

    (let ((find-command (find-cmd '(prune (name ".git"))
                                  '(name "*.elc"))))

      (shell-command find-command t t))

    (goto-char (point-min))

    (setq more-lines t)

    (while more-lines
      (let ((start (progn (beginning-of-line)
                          (point)))
            (end (progn (end-of-line)
                        (point))))
        (let ((el (buffer-substring start (- end 1)))
              (elc (buffer-substring start end)))

          (if (file-newer-than-file-p el elc)
              (byte-compile-file (buffer-substring start (- end 1))))))
      (setq more-lines (= 0 (forward-line 1))))))
