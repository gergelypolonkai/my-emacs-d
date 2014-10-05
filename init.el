(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(background-color "#7f7f7f")
 '(background-mode dark)
 '(blink-cursor-mode t)
 '(column-number-mode t)
 '(cursor-color "#5c5cff")
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(ediff-merge-split-window-function (quote split-window-horizontally))
 '(ediff-split-window-function (quote split-window-vertically))
 '(foreground-color "#5c5cff")
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(inhibit-startup-screen t)
 '(nxml-attribute-indent 4)
 '(nxml-child-indent 2)
 '(nxml-outline-child-indent 4)
 '(cursor-type 'bar)
 '(tab-width 4)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("e6h" . "http://www.e6h.org/packages/"))))
 '(safe-local-variable-values (quote ((company-clang-arguments "-I.." "-I/home/polesz/jhbuild/install/include" "-I/home/polesz/jhbuild/install/include/atk-1.0" "-I/home/polesz/jhbuild/install/include/at-spi-2.0" "-I/home/polesz/jhbuild/install/include/at-spi2-atk/2.0" "-I/home/polesz/jhbuild/install/include/cairo" "-I/home/polesz/jhbuild/install/include/gdk-pixbuf-2.0" "-I/home/polesz/jhbuild/install/include/gio-unix-2.0/" "-I/home/polesz/jhbuild/install/include/glib-2.0" "-I/home/polesz/jhbuild/install/include/gtk-3.0" "-I/home/polesz/jhbuild/install/include/harfbuzz" "-I/home/polesz/jhbuild/install/include/libgda-5.0" "-I/home/polesz/jhbuild/install/include/libgda-5.0/libgda" "-I/home/polesz/jhbuild/install/include/librsvg-2.0" "-I/home/polesz/jhbuild/install/include/libsoup-2.4" "-I/home/polesz/jhbuild/install/include/pango-1.0" "-I/home/polesz/jhbuild/install/include/swe-glib" "-I/home/polesz/jhbuild/install/include/webkitgtk-4.0" "-I/home/polesz/jhbuild/install/lib/glib-2.0/include" "-I/usr/include/dbus-1.0" "-I/usr/include/freetype2" "-I/usr/include/libdrm" "-I/usr/include/libpng16" "-I/usr/include/libxml2" "-I/usr/include/pixman-1" "-I/usr/lib64/dbus-1.0/include") (company-clang-arguments "-I/home/polesz/jhbuild/install/include" "-I/home/polesz/jhbuild/install/include/atk-1.0" "-I/home/polesz/jhbuild/install/include/at-spi-2.0" "-I/home/polesz/jhbuild/install/include/at-spi2-atk/2.0" "-I/home/polesz/jhbuild/install/include/cairo" "-I/home/polesz/jhbuild/install/include/gdk-pixbuf-2.0" "-I/home/polesz/jhbuild/install/include/gio-unix-2.0/" "-I/home/polesz/jhbuild/install/include/glib-2.0" "-I/home/polesz/jhbuild/install/include/gtk-3.0" "-I/home/polesz/jhbuild/install/include/harfbuzz" "-I/home/polesz/jhbuild/install/include/libgda-5.0" "-I/home/polesz/jhbuild/install/include/libgda-5.0/libgda" "-I/home/polesz/jhbuild/install/include/librsvg-2.0" "-I/home/polesz/jhbuild/install/include/libsoup-2.4" "-I/home/polesz/jhbuild/install/include/pango-1.0" "-I/home/polesz/jhbuild/install/include/swe-glib" "-I/home/polesz/jhbuild/install/include/webkitgtk-4.0" "-I/home/polesz/jhbuild/install/lib/glib-2.0/include" "-I/usr/include/dbus-1.0" "-I/usr/include/freetype2" "-I/usr/include/libdrm" "-I/usr/include/libpng16" "-I/usr/include/libxml2" "-I/usr/include/pixman-1" "-I/usr/lib64/dbus-1.0/include") (company-clang-arguments "-I/usr/include/glib-2.0")))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(my-long-line-face ((((class color)) (:background "gray10"))) t)
 '(my-tab-face ((((class color)) (:background "grey10"))) t)
 '(my-trailing-space-face ((((class color)) (:background "gray10"))) t))

(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/nyan-mode")
(add-to-list 'load-path "~/.emacs.d/emacs-async")
(add-to-list 'load-path "~/.emacs.d/helm")
(add-to-list 'load-path "~/.emacs.d/emacs-helm-gtags")
(add-to-list 'load-path "~/.emacs.d/move-line")

; Nyanyanyanyanya
(require 'nyan-mode)
(require 'helm-config)
(require 'helm-gtags)
(require 'move-line)
(require 'whitespace)
(require 'rcirc)

(nyan-mode t)

; OrgMode mobileness
(setq org-directory "~/Dokumentumok/org")
(setq org-mobile-directory "~/Dokumentumok/org")
(setq org-mobile-inbox-for-pull "~/Dokumentumok/org/inbox.org")

(load "gnu-c-header.el")
(load "gobgen/gobgen.el")
(load "toggle-window-split.el")
;;(load "elpa/move-line-0.0.1/move-line.el")

(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'which-func-mode)

(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))
(global-set-key (kbd "C-x f") 'fiplr-find-file)
(global-set-key (kbd "C-x _") 'maximize-window)
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))
(global-set-key (kbd "M-up") 'move-line-up)
(global-set-key (kbd "M-down") 'move-line-down)

(setq whitespace-style '(tabs trailing lines tab-mark))
(setq whitespace-line-column 78)
(global-whitespace-mode 1)
(setq-default indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(add-hook 'font-lock-mode-hook
    (function
        (lambda ()
            (setq font-lock-keywords
                (append font-lock-keywords
                    '(
                        ("\t+" (0 'my-tab-face t))
                        ("^.\\{81,\\}$" (0 'my-long-line-face t))
                        ("[ \t]+$"      (0 'my-trailing-space-face t))
                    )
                )
            )
        )
    )
)
(c-add-style "my"
             '(
               (c-basic-offset . 4)
               (c-offsets-alist
                (arglist-cont . 0)
                (arglist-intro . ++)
                (block-close . 0)
                (brace-entry-open . 0)
                (brace-list-close . 0)
                (brace-list-intro . +)
                (case-label . +)
                (class-close . 0)
                (defun-block-intro . +)
                (defun-close . 0)
                (defun-open . 0)
                (inclass . +)
                (statement . 0)
                (statement-block-intro . +)
                (statement-case-intro . +)
                (statement-cont . 4)
                (substatement-open . 0)
                (topmost-intro . 0)
                (topmost-intro-cont . 0)
                (access-label . -)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (arglist-close . c-lineup-close-paren)
                (arglist-cont-nonempty . c-lineup-arglist)
                (block-open . 0)
                (brace-list-entry . 0)
                (brace-list-open . 0)
                (c . c-lineup-C-comments)
                (catch-clause . 0)
                (class-open . 0)
                (comment-intro . c-lineup-comment)
                (composition-close . 0)
                (composition-open . 0)
                (cpp-define-intro c-lineup-cpp-define +)
                (cpp-macro . -1000)
                (cpp-macro-cont . +)
                (do-while-closure . 0)
                (else-clause . 0)
                (extern-lang-close . 0)
                (extern-lang-open . 0)
                (friend . 0)
                (func-decl-cont . +)
                (incomposition . +)
                (inexpr-class . +)
                (inexpr-statement . +)
                (inextern-lang . +)
                (inher-cont . c-lineup-multi-inher)
                (inher-intro . +)
                (inlambda . c-lineup-inexpr-block)
                (inline-close . 0)
                (inline-open . +)
                (inmodule . +)
                (innamespace . +)
                (knr-argdecl . 0)
                (knr-argdecl-intro . +)
                (label . 2)
                (lambda-intro-cont . +)
                (member-init-cont . c-lineup-multi-inher)
                (member-init-intro . +)
                (module-close . 0)
                (module-open . 0)
                (namespace-close . 0)
                (namespace-open . 0)
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (objc-method-intro .
                                   [0])
                (statement-case-open . 0)
                (stream-op . c-lineup-streamop)
                (string . -1000)
                (substatement . +)
                (substatement-label . 2)
                (template-args-cont c-lineup-template-args +))))
(add-hook 'after-init-hook 'global-company-mode)
(setq rcirc-default-nick "GergelyPolonkai")
(setq rcirc-default-user-name "polesz")
(setq rcirc-default-full-name "Gergely Polonkai")
(add-hook 'after-init-hook 'fiplr-clear-cache)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
