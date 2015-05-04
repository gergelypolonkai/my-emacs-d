;;; mark-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "mark" "mark.el" (21831 16364 152207 38000))
;;; Generated autoloads from mark.el

(autoload 'backward-mark "mark" "\
Moves the point arg points backward in the mark ring.

\(fn ARG)" t nil)

(autoload 'forward-mark "mark" "\
Moves the point arg points forward in the mark ring.

\(fn ARG)" t nil)

(autoload 'mark-mode-goto "mark" "\
Go to the occurrence the current line describes.

\(fn)" t nil)

(autoload 'mark-mode-delete "mark" "\
Delete mark at current line from mark-ring.

\(fn)" t nil)

(autoload 'mark-mode-prev-mark "mark" "\
Move to previous mark in *mark* buffer, wrapping if necessary.

\(fn)" t nil)

(autoload 'mark-mode-next-mark "mark" "\
Move to next mark in *mark* buffer, wrapping if necessary.

\(fn)" t nil)

(autoload 'show-marks "mark" "\
Displays all the lines for each point in the mark ring.  Pressing
RET in the result buffer will send you to corresponding mark point
with out affecting the mark-ring.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; mark-autoloads.el ends here
