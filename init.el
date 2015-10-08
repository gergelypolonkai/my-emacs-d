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
 '(cursor-type (quote bar))
 '(custom-enabled-themes (quote (tango-dark)))
 '(custom-safe-themes
   (quote
    ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(ediff-merge-split-window-function (quote split-window-horizontally))
 '(ediff-split-window-function (quote split-window-vertically))
 '(fiplr-ignored-globs
   (quote
    ((directories
      (".git" ".svn" ".hg" ".bzr"))
     (files
      (".#*" "*.so" "*~")))))
 '(foreground-color "#5c5cff")
 '(global-hl-line-mode t)
 '(global-whitespace-mode t)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(inhibit-startup-screen t)
 '(nxml-attribute-indent 4)
 '(nxml-child-indent 2)
 '(nxml-outline-child-indent 4)
 '(nyan-wavy-trail t)
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("e6h" . "http://www.e6h.org/packages/"))))
 '(safe-local-variable-values
   (quote
    ((company-clang-arguments "-I.." "-I/home/polesz/jhbuild/install/include/atk-1.0" "-I/home/polesz/jhbuild/install/include/at-spi-2.0" "-I/home/polesz/jhbuild/install/include/at-spi2-atk/2.0" "-I/home/polesz/jhbuild/install/include/cairo" "-I/home/polesz/jhbuild/install/include/gdk-pixbuf-2.0" "-I/home/polesz/jhbuild/install/include/gio-unix-2.0/" "-I/home/polesz/jhbuild/install/include/glib-2.0" "-I/home/polesz/jhbuild/install/include/gtk-3.0" "-I/home/polesz/jhbuild/install/include/harfbuzz" "-I/home/polesz/jhbuild/install/include/libgda-5.0" "-I/home/polesz/jhbuild/install/include/libgda-5.0/libgda" "-I/home/polesz/jhbuild/install/include/librsvg-2.0" "-I/home/polesz/jhbuild/install/include/libsoup-2.4" "-I/home/polesz/jhbuild/install/include/pango-1.0" "-I/home/polesz/jhbuild/install/include/swe-glib" "-I/home/polesz/jhbuild/install/include/webkitgtk-4.0" "-I/home/polesz/jhbuild/install/lib/glib-2.0/include" "-I/usr/include/dbus-1.0" "-I/usr/include/freetype2" "-I/usr/include/libdrm" "-I/usr/include/libpng16" "-I/usr/include/libxml2" "-I/usr/include/pixman-1" "-I/usr/lib64/dbus-1.0/include")
     (company-clang-arguments "-I.." "-I/home/polesz/jhbuild/install/include" "-I/home/polesz/jhbuild/install/include/atk-1.0" "-I/home/polesz/jhbuild/install/include/at-spi-2.0" "-I/home/polesz/jhbuild/install/include/at-spi2-atk/2.0" "-I/home/polesz/jhbuild/install/include/cairo" "-I/home/polesz/jhbuild/install/include/gdk-pixbuf-2.0" "-I/home/polesz/jhbuild/install/include/gio-unix-2.0/" "-I/home/polesz/jhbuild/install/include/glib-2.0" "-I/home/polesz/jhbuild/install/include/gtk-3.0" "-I/home/polesz/jhbuild/install/include/harfbuzz" "-I/home/polesz/jhbuild/install/include/libgda-5.0" "-I/home/polesz/jhbuild/install/include/libgda-5.0/libgda" "-I/home/polesz/jhbuild/install/include/librsvg-2.0" "-I/home/polesz/jhbuild/install/include/libsoup-2.4" "-I/home/polesz/jhbuild/install/include/pango-1.0" "-I/home/polesz/jhbuild/install/include/swe-glib" "-I/home/polesz/jhbuild/install/include/webkitgtk-4.0" "-I/home/polesz/jhbuild/install/lib/glib-2.0/include" "-I/usr/include/dbus-1.0" "-I/usr/include/freetype2" "-I/usr/include/libdrm" "-I/usr/include/libpng16" "-I/usr/include/libxml2" "-I/usr/include/pixman-1" "-I/usr/lib64/dbus-1.0/include")
     (company-clang-arguments "-I/home/polesz/jhbuild/install/include" "-I/home/polesz/jhbuild/install/include/atk-1.0" "-I/home/polesz/jhbuild/install/include/at-spi-2.0" "-I/home/polesz/jhbuild/install/include/at-spi2-atk/2.0" "-I/home/polesz/jhbuild/install/include/cairo" "-I/home/polesz/jhbuild/install/include/gdk-pixbuf-2.0" "-I/home/polesz/jhbuild/install/include/gio-unix-2.0/" "-I/home/polesz/jhbuild/install/include/glib-2.0" "-I/home/polesz/jhbuild/install/include/gtk-3.0" "-I/home/polesz/jhbuild/install/include/harfbuzz" "-I/home/polesz/jhbuild/install/include/libgda-5.0" "-I/home/polesz/jhbuild/install/include/libgda-5.0/libgda" "-I/home/polesz/jhbuild/install/include/librsvg-2.0" "-I/home/polesz/jhbuild/install/include/libsoup-2.4" "-I/home/polesz/jhbuild/install/include/pango-1.0" "-I/home/polesz/jhbuild/install/include/swe-glib" "-I/home/polesz/jhbuild/install/include/webkitgtk-4.0" "-I/home/polesz/jhbuild/install/lib/glib-2.0/include" "-I/usr/include/dbus-1.0" "-I/usr/include/freetype2" "-I/usr/include/libdrm" "-I/usr/include/libpng16" "-I/usr/include/libxml2" "-I/usr/include/pixman-1" "-I/usr/lib64/dbus-1.0/include")
     (company-clang-arguments "-I/usr/include/glib-2.0"))))
 '(sgml-basic-offset 4)
 '(show-trailing-whitespace t)
 '(tab-width 4)
 '(jekyll-directory "~/Projektek/jekyll/gergely.polonkai.eu"))

(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(set-face-attribute 'default t :font "Hack-10")
(set-frame-font "Hack-10" nil t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit nil :background "gray25"))))
 '(trailing-whitespace ((t (:inherit nil :background "red1"))))
 '(whitespace-line ((t (:inherit nil :background "orange")))))

(add-to-list 'load-path (concat user-emacs-directory "nyan-mode"))
(add-to-list 'load-path (concat user-emacs-directory "emacs-async"))
(add-to-list 'load-path (concat user-emacs-directory "helm"))
(add-to-list 'load-path (concat user-emacs-directory "emacs-helm-gtags"))
(add-to-list 'load-path (concat user-emacs-directory "move-line"))
(add-to-list 'load-path (concat user-emacs-directory "ggtags"))

; Nyanyanyanyanya
(require 'nyan-mode)
(require 'helm-config)
(require 'helm-gtags)
(require 'move-line)
(require 'whitespace)
(require 'rcirc)
(require 'ggtags)
(require 'thingatpt)

(nyan-mode t)

; OrgMode mobileness
(setq org-directory "~/Dokumentumok/org")
(setq org-mobile-directory "~/Dokumentumok/org")
(setq org-mobile-inbox-for-pull "~/Dokumentumok/org/inbox.org")

(load (concat user-emacs-directory "gnu-c-header.el"))
(load (concat user-emacs-directory "gobgen/gobgen.el"))
(load (concat user-emacs-directory "toggle-window-split.el"))
(load (concat user-emacs-directory "round-number-to-decimals.el"))
(load (concat user-emacs-directory "transpose-windows.el"))
(load (concat user-emacs-directory "zim.el"))
(load (concat user-emacs-directory "clearcase.el"))
(load (concat user-emacs-directory "jekyll.el"))

(add-hook 'c-mode-hook
          (lambda ()
            (helm-gtags-mode)
            (which-func-mode)
            (flyspell-prog-mode)))
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
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
;; Some terminals don’t interpret Alt-Up/Down as M-<up/down>.
(global-set-key (kbd "ESC <up>") 'move-line-up)
(global-set-key (kbd "ESC <down>") 'move-line-down)

(global-whitespace-mode 1)
(setq-default indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;(c-add-style "my"
;             '(
;               (c-basic-offset . 4)
;               (c-offsets-alist
;                (arglist-cont . 0)
;                (arglist-intro . ++)
;                (block-close . 0)
;                (brace-entry-open . 0)
;                (brace-list-close . 0)
;                (brace-list-intro . +)
;                (case-label . +)
;                (class-close . 0)
;                (defun-block-intro . +)
;                (defun-close . 0)
;                (defun-open . 0)
;                (inclass . +)
;                (statement . 0)
;                (statement-block-intro . +)
;                (statement-case-intro . +)
;                (statement-cont . 4)
;                (substatement-open . 0)
;                (topmost-intro . 0)
;                (topmost-intro-cont . 0)
;                (access-label . -)
;                (annotation-top-cont . 0)
;                (annotation-var-cont . +)
;                (arglist-close . c-lineup-close-paren)
;                (arglist-cont-nonempty . c-lineup-arglist)
;                (block-open . 0)
;                (brace-list-entry . 0)
;                (brace-list-open . 0)
;                (c . c-lineup-C-comments)
;                (catch-clause . 0)
;                (class-open . 0)
;                (comment-intro . c-lineup-comment)
;                (composition-close . 0)
;                (composition-open . 0)
;                (cpp-define-intro c-lineup-cpp-define +)
;                (cpp-macro . -1000)
;                (cpp-macro-cont . +)
;                (do-while-closure . 0)
;                (else-clause . 0)
;                (extern-lang-close . 0)
;                (extern-lang-open . 0)
;                (friend . 0)
;                (func-decl-cont . +)
;                (incomposition . +)
;                (inexpr-class . +)
;                (inexpr-statement . +)
;                (inextern-lang . +)
;                (inher-cont . c-lineup-multi-inher)
;                (inher-intro . +)
;                (inlambda . c-lineup-inexpr-block)
;                (inline-close . 0)
;                (inline-open . +)
;                (inmodule . +)
;                (innamespace . +)
;                (knr-argdecl . 0)
;                (knr-argdecl-intro . +)
;                (label . 2)
;                (lambda-intro-cont . +)
;                (member-init-cont . c-lineup-multi-inher)
;                (member-init-intro . +)
;                (module-close . 0)
;                (module-open . 0)
;                (namespace-close . 0)
;                (namespace-open . 0)
;                (objc-method-args-cont . c-lineup-ObjC-method-args)
;                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
;                (objc-method-intro .
;                                   [0])
;                (statement-case-open . 0)
;                (stream-op . c-lineup-streamop)
;                (string . -1000)
;                (substatement . +)
;                (substatement-label . 2)
;                (template-args-cont c-lineup-template-args +))))

(defun my-c-initialization-hook ()
  (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-c-initialization-hook)
(setq c-offset-alist '((member-init-intro . ++)))

(defconst my-c-style
  '((c-tab-always-indent        . t)
    (c-comment-only-line-offset . 4)
    (c-hanging-braces-alist     . ((substatement-open after)
                                   (brace-list-open)))
    (c-hanging-colons-alist     . ((member-init-intro before)
                                   (inher-intro)
                                   (case-label after)
                                   (label after)
                                   (access-label after)))
    (c-cleanup-list             . (scope-operator
                                   empty-defun-braces
                                   defun-close-semi))
    (c-offsets-alist             . ((arglist-close . +)
                                    (arglist-intro . ++)
                                    (substatement-open . 0)
                                    (case-label . 4)
                                    (block-open . 0)
                                    (knr-argdecl-intro . -)
                                    (comment-intro . 0)))
    (c-echo-syntactic-information-p . t))
  "My C Programming Style")
(c-add-style "PERSONAL" my-c-style)
(defun my-c-mode-common-hook ()
  (c-set-style "PERSONAL")
  (setq tab-width 4
        indent-tabs-mode nil)
  (c-toggle-auto-newline 1))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))
(add-hook 'after-init-hook 'fiplr-clear-cache)

(setq rcirc-default-nick "GPolonkai")
(setq rcirc-default-user-name "polesz")
(setq rcirc-default-full-name "Gergely Polonkai")

(defun delete-current-line ()
  "Kill the whole line on which point is"
  (interactive)
  (beginning-of-line)
  (kill-line 1))

(defun copy-func-prototype ()
  "Copy the current function's prototype to the kill-ring"

  (interactive)

  (save-excursion (beginning-of-defun)
                  (setq protocopy-begin (point))
                  (forward-list)
                  (setq protocopy-end (point))
                  (kill-ring-save protocopy-begin protocopy-end)))

(defun duplicate-line()
  (interactive)
  (save-excursion
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (next-line 1)
    (yank)))

(global-set-key (kbd "C-c C-y") 'duplicate-line)

(add-to-list 'auto-mode-alist '("\\.vala\\'" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(defun toggle-char-case (arg-move-point)
  "Toggle the case of the char after point. Based on Xah's toggle letter
case defun version 2015-12-22

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2016-02-16"
  (interactive "P")
  (let ((case-fold-search nil))
    (cond
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))
    (cond
     (arg-move-point (right-char)))))
