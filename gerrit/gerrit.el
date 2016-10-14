(eval-when-compile (require 'cl))

(require 'request)

(defun gerrit-json-read ()
  "Gerrit puts five extra chars (\")]}'\n\")at the beginning of
  each JSON response for security reasons. All this function does
  is it strips those chars before calling `json-read'"

  (goto-line 2)
  (beginning-of-line)
  (json-read))

(let ((username "epolger")
      (password "EsthaiTh6Fu"))
  (request
   "https://gerrit.ericsson.se/a/changes/?q=status:open+owner:self"
   :parser 'gerrit-json-read
   :headers '(("Authorization"
               (concat ("Basic "
                        (base64-encode-string (concat username
                                                      ":"
                                                      password))))))
   :success (function* (lambda (&key data &allow-other-keys)
                         (message "Success!")
                         (message "%s" data)))
   :error (function* (lambda (&key error-thrown &allow-other-keys&rest _)
                       (message "Got error: %s" error-thrown)))
   :complete (lambda (&rest _) (message "Finished!"))
   :status-code '((401 . (lambda (&rest _) (message "Got 401"))))))
