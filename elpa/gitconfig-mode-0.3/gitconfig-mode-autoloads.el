;;; gitconfig-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "gitconfig-mode" "gitconfig-mode.el" (21633
;;;;;;  45695 432043 861000))
;;; Generated autoloads from gitconfig-mode.el

(autoload 'gitconfig-mode "gitconfig-mode" "\
A major mode for editing .gitconfig files.

\(fn)" t nil)

(setq auto-mode-alist (append '(("/\\.gitconfig\\'" . gitconfig-mode) ("/\\.git/config\\'" . gitconfig-mode)) auto-mode-alist))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; gitconfig-mode-autoloads.el ends here
