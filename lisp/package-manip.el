;;; package-manip.el --- Utility functions to check if package upgrades are available

;;; Commentary:
;; I should add one.

;; Credits go to http://emacs.stackexchange.com/a/16407/507

;;; Code:

(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                (let ((pkg (cadr (assq name where))))
                  (when pkg
                    (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (message "All packages are up to date"))))

(defun check-todays-package-upgrade-p (&optional no-save)
  "Check if automatic package upgrade has been performed today.

This function reads the date of the last check from the
\"last-package-upgrade\" file. Where this file is looked for is
guessed as follows:

If `user-emacs-cache-directory' is set (e.g. by the
`xdg-paths.el' package available from
https://github.com/tomprince/xdg-paths-el), the timestamp file is
opened from there. Otherwise, it will be read from
`user-emacs-directory'

If NO-SAVE is 'nil', the current date will be saved to the
timestamp file.

The return value of this function will be 't' if the timestamp
file contains todays date, 'nil' otherwise."

  (let ((tsfile-location (if (boundp 'user-emacs-cache-directory)
                             user-emacs-cache-directory
                           user-emacs-directory)))
    (unless (and (file-exists-p tsfile-location)
                 (file-accessible-directory-p tsfile-location))
      (make-directory tsfile-location t))

    (let ((timestamp-today t)
          (timestamp-buffer (find-file-literally
                             (expand-file-name "last-package-upgrade"
                                               tsfile-location))))
      (with-current-buffer timestamp-buffer
        (goto-char 0)
        (unless (looking-at-p (format-time-string "%Y-%m-%d"))
          (unless no-save
            (erase-buffer)
            (insert (format-time-string "%Y-%m-%d"))
            (save-buffer 0))
          (setq timestamp-today nil))
        (kill-buffer)
        timestamp-today))))

(defun check-for-package-upgrades-on-day (&optional day-number)
  "Check for package upgrades if today is DAY-NUMBER.

DAY-NUMBER can be anything between 0 and 7, inclusive.  Both 0
and 7 denote Sunday to make both types of users happy.

If DAY-NUMBER is 'nil', it defaults to today's day."

  (let ((actual-day-number (cond
                            ((eq day-number nil) (string-to-number (format-time-string "%u")))
                            ((eq day-number 0) 7)
                            (t day-number))))
    (when (and (eq day-number (string-to-number (format-time-string "%u")))
               (not (check-todays-package-upgrade-p t)))
      (message "Calling")
      (when (call-interactively 'package-upgrade-all)
        (message "Called")
        (check-todays-package-upgrade-p nil)))))

(provide 'package-manip)

;;; package-manip.el ends here
