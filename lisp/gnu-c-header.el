;;; gnu-c-header.el --- function to add a header to the C files licensed under the GPL
;; Author: Gergely Polonkai <gergely@polonkai.eu>
;; Copyright: public domain
;; URL: http://gergely.polonkai.eu/blog/2014/09/26/emacs-adding-gnu-c-headers
;; Keywords: gnu, c, header, helper, utilities
;;; Commentary:
;;; Code:

(defun file-name-base-with-extension (&optional filename)
  "Return the base name of the FILENAME without its directory path.
FILENAME defaults to `buffer-file-name'."
   (file-name-nondirectory (or filename (buffer-file-name))))

(defun add-gnu-c-header (progname purpose)
  "Add a GNU GPL header to the current buffer"
  (interactive "sProgram name (ie: MyProgram): \nsPurpose of the file (ie: string utility functions): ")
  (save-excursion
    (goto-char (point-min))
    (insert (concat
             "/* "
             (file-name-base-with-extension buffer-file-name)
             " - "
             purpose
             " for "
             progname
             "\n"
             " *\n"
             " * Copyright (C) "
             (format-time-string "%Y" (current-time))
             " "
             (user-full-name)
             "\n"
             " *\n"))
    (let ((start (point)))
      (insert (concat
               " * "
               progname
               " is \n * free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 3 of the License, or (at your option) any later version."))
      (let ((end (point)))
        (fill-region start end)))
    (insert (concat
             "\n"
             " *\n"))
    (let ((start (point)))
      (insert (concat
               " * "
               progname
               " is \n * distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details."))
      (let ((end (point)))
        (fill-region start end)))
    (insert (concat
             "\n"
             " *\n"))
    (let ((start (point)))
      (insert " * You \n * should have received a copy of the GNU General Public License along with this software; if not, see <http://www.gnu.org/licenses/>.")
      (let ((end (point)))
        (fill-region start end)))
    (insert (concat
             "\n"
             " */\n"))
    ))

(provide 'gnu-c-header)

;;; gnu-c-header.el ends here
