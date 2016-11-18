;; id-manager extensions

(defun gpolonkai/idm-record-get-field (record field)
  "Get FIELD of an id-manager RECORD."
  (let ((funcname (intern (concat "idm-record-" (symbol-name field)))))
    (when (fboundp funcname)
      (funcall funcname record))))

(defun gpolonkai/idm-get-field-for-account (account field)
  "Get id-manager password for ACCOUNT."
  (let ((db (idm-load-db))
        (lookup-record nil))
    (dolist (record (funcall db 'get-all-records) password)
      (when (string= account (idm-record-name record))
        (setq lookup-record (gpolonkai/idm-record-get-field record field))))
    lookup-record))

(defmacro gpolonkai/idm-get-password-for-account (account)
  `(gpolonkai/idm-get-field-for-account ,account 'password))

(defmacro gpolonkai/idm-get-id-for-account (account)
  `(gpolonkai/idm-get-field-for-account ,account 'account-id))
