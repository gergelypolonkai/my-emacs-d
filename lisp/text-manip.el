;;; package --- Summary

;;; Commentary:

;;; Code:
(defun org-space-key (&optional arg)
  "Insert two spaces after a period.

ARG will be passed down verbatim to `self-insert-command'"
  (interactive "p")

  (when (looking-back "[.!?…]" nil)
    (call-interactively 'self-insert-command arg))
  (call-interactively 'self-insert-command arg))

;; From http://pages.sachachua.com/.emacs.d/Sacha.html
(defun sachachua/fill-or-unfill-paragraph (&optional unfill region)
  "Fill (or unfill, if UNFILL is non-nil) paragraph (or REGION)."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
    (fill-paragraph nil region)))

;; Copied from http://emacs.stackexchange.com/a/27170/507
(defun so/query-swap-strings (from-string
                              to-string
                              &optional delimited start end)
  "Swap occurrences of FROM-STRING and TO-STRING.

DELIMITED, START, and END are passed down verbatim to `perform-replace'."
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Query swap"
                   (if current-prefix-arg
                       (if (eq current-prefix-arg '-) " backward" " word")
                     "")
                   (if (use-region-p) " in region" ""))
           nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common)
           (if (use-region-p) (region-beginning))
           (if (use-region-p) (region-end)))))
  (perform-replace
   (concat "\\(" (regexp-quote from-string) "\\)\\|" (regexp-quote to-string))
   `(replace-eval-replacement replace-quote
                              (if (match-string 1)
                                  ,to-string
                                ,from-string))
   t t delimited nil nil start end))

(provide 'text-manip)

;;; text-manip.el ends here
