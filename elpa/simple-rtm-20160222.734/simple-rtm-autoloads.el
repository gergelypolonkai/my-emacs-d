;;; simple-rtm-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "simple-rtm" "simple-rtm.el" (22533 17554 588844
;;;;;;  77000))
;;; Generated autoloads from simple-rtm.el
 (put 'simple-rtm-mode-line-string 'risky-local-variable t)

(autoload 'simple-rtm-mode "simple-rtm" "\
An interactive \"do everything right now\" mode for Remember The Milk

Display all of your lists and tasks in a new buffer or switch to
that buffer if it already exists.

Each action will be sent to the Remember The Milk web interface
immediately.

\\{simple-rtm-mode-map}

\(fn)" t nil)

(defvar display-simple-rtm-tasks-mode nil "\
Non-nil if Display-Simple-Rtm-Tasks mode is enabled.
See the `display-simple-rtm-tasks-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `display-simple-rtm-tasks-mode'.")

(custom-autoload 'display-simple-rtm-tasks-mode "simple-rtm" nil)

(autoload 'display-simple-rtm-tasks-mode "simple-rtm" "\
Display SimpleRTM task statistics in the mode line.
The text being displayed in the mode line is controlled by the variables
`simple-rtm-mode-line-format'.
The mode line will be updated automatically when a task is modified.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; simple-rtm-autoloads.el ends here
