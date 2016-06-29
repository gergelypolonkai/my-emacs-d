;;; django-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "django-html-mode" "django-html-mode.el" (22387
;;;;;;  29372 428700 65000))
;;; Generated autoloads from django-html-mode.el

(autoload 'django-html-mode "django-html-mode" "\
Major mode for editing Django html templates (.djhtml).

\\{django-html-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.djhtml$" . django-html-mode))

;;;***

;;;### (autoloads nil "django-mode" "django-mode.el" (22387 29372
;;;;;;  420706 182000))
;;; Generated autoloads from django-mode.el

(autoload 'django-mode "django-mode" "\
Major mode for Django web framework.

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\<\\(models\\|views\\|handlers\\|feeds\\|sitemaps\\|admin\\|context_processors\\|urls\\|settings\\|tests\\|assets\\|forms\\)\\.py\\'" . django-mode))

;;;***

;;;### (autoloads nil nil ("django-mode-pkg.el") (22387 29372 445484
;;;;;;  219000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; django-mode-autoloads.el ends here
