(defun warn-key-rebind (keymap key new-def)
  "Warn if a key is being rebound."
  (let ((global-def (global-key-binding key))
        (local-def (local-key-binding key))
        (minor-defs (minor-mode-key-binding key))
        (key-desc (key-description key)))
    (when (and global-def
               (not (numberp global-def))
               (not (equal global-def new-def)))
      (warn "'%s' from the global keymap is being rebound from '%s' to '%s'"
            key-desc
            global-def
            new-def))
    (when (and local-def
               (not (numberp local-def))
               (not (equal local-def new-def)))
      (warn "'%s' from the local keymap is being rebound from '%s' to '%s'"
            key-desc
            local-def
            new-def))
    (when minor-defs
      (dolist (binding minor-defs)
        (warn "'%s' from '%s' keymap is being rebound from '%s' to '%s'"
              key-desc
              (car binding)
              (cdr binding)
              new-def)))))
