;;; flycheck-swagger-tools --- Flycheck checker for swagger-tools.

;; Copyright (C) 2017 Marc-André Goyette
;; Author: Marc-André Goyette <goyette.marcandre@gmail.com>
;; URL: https://github.com/magoyette/flycheck-swagger-tools
;; Version: 0.1.0
;; Package-Requires: ((emacs "25"))
;; Keywords: swagger

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; flycheck-swagger-tools provides a Flycheck checker for swagger-tools.
;; This allows to validate Swagger YAML and JSON files.

;; The checker can be activating by requiring this package.

;; (require 'flycheck-swagger-tools)

;; By default, only the first 4000 characters of a file are scanned to
;; find the swagger 2.0 element.  To avoid stack overflow in Emacs
;; multi-line regex, this value is necessary.  The defcustom
;; swagger-tools-predicate-regexp-match-limit can be used to change this
;; limit.  That could be necessary for YAML files with long initial comments.

;;; Code:

(require 'flycheck)

(defgroup swagger-tools nil
  "Validate swagger files with swagger-tools."
  :group 'swagger
  :prefix "swagger-tools-")

(defcustom swagger-tools-predicate-regexp-match-limit 4000
  "Defines the number of characters that will be scanned at the beginning of a
buffer to find the swagger 2.0 element."
  :type 'integer
  :group 'swagger-tools)

;;;###autoload
(flycheck-define-checker swagger-tools
  "A checker that uses swagger tools to validate Swagger2 JSON and YAML files.
See URL `https://github.com/apigee-127/swagger-tools'."
  :command ("swagger-tools" "validate" source)
  :predicate
  (lambda ()
    (string-match
     "\\(.\\|\n\\)*\\([[:space:]]\\|\"\\|\\'\\)*swagger\\([[:space:]]\\|\"\\|\\'\\)*:[[:space:]]*[\"\\']2.0[\"\\'].*"
     ;; Need to avoid stack overflow for multi-line regex
     (buffer-substring 1 (min (buffer-size)
                              swagger-tools-predicate-regexp-match-limit))))
  :error-patterns
  ((warning line-start "  " (message (one-or-more not-newline)
                                     "is not used"
                                     (one-or-more not-newline))
            line-end)
   ;; js-yaml error with position
   (error line-start "  error: " (message) " at line " line ", column " column ":" line-end)
   ;; js-yaml error without position
   (error line-start "  error: " (message) ": " (one-or-more not-newline) line-end)
   ;; swagger-tools error (always contain a # symbol in the message)
   (error line-start
          "  "
          (message (optional (one-or-more not-newline))
                   (one-or-more "#")
                   (one-or-more not-newline))
          line-end))
  :error-filter
  ;; Add line number 1 if the error has no line number
  (lambda (errors)
    (let ((errors (flycheck-sanitize-errors errors)))
      (dolist (err errors)
        (unless (flycheck-error-line err)
          (setf (flycheck-error-line err) 1)))
      errors))
  :modes (json-mode openapi-yaml-mode yaml-mode))

(add-to-list 'flycheck-checkers 'swagger-tools)

(provide 'flycheck-swagger-tools)
;;; flycheck-swagger-tools.el ends here
