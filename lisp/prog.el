;;; gp-prog --- Summary

;;; Commentary:

;;; Code:
(defun gpolonkai/prog-in-string-p ()
  "Return t if point is inside a string."
  (nth 3 (syntax-ppss)))

(defun gpolonkai/prog-in-comment-p ()
  "Return t if point is inside a comment."
  (nth 4 (syntax-ppss)))

(defun gpolonkai/python-add-docstring ()
  "Add a Python docstring to the current thing.

If point is inside a function, add docstring to that.  If point
is in a class, add docstring to that.  If neither, add docstring
to the beginning of the file."
  (interactive)
  (save-restriction
    (widen)
    (beginning-of-defun)
    (if (not (looking-at-p "\\(def\\|class\\) "))
        (progn
          (goto-char (point-min))
          (back-to-indentation)
          (forward-char)
          (while (gpolonkai/prog-in-comment-p)
            (forward-line)
            (back-to-indentation)
            (forward-char)))
      (search-forward ":")
      (while (or (gpolonkai/prog-in-string-p)
                 (gpolonkai/prog-in-comment-p))
        (search-forward ":")))
    (if (eq 1 (count-lines 1 (point)))
        (open-line-above)
      (open-line-below))
    (insert "\"\"\"")
    (open-line-below)
    (insert "\"\"\"")
    (open-line-above)))

(provide 'gp-prog)

;;; gp-prog.el ends here
