(package-initialize)

(add-to-list 'load-path (concat
                         user-emacs-directory
                         (convert-standard-filename "lisp/")))
(require 'xdg-paths)

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
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
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
 '(jekyll-directory "~/Projektek/jekyll/gergely.polonkai.eu")
 '(nxml-attribute-indent 4)
 '(nxml-child-indent 2)
 '(nxml-outline-child-indent 4)
 '(nyan-wavy-trail t)
 '(org-crypt-key "B0740C4C")
 '(org-default-notes-file
   (concat user-documents-directory
           (convert-standard-filename "/orgmode/notes.org")))
 '(org-directory
   (concat user-documents-directory
           (convert-standard-filename "/orgmode/")))
 '(org-ellipsis "…#")
 '(org-mobile-directory
   (concat user-documents-directory
           (convert-standard-filename "/orgmode/")))
 '(org-mobile-inbox-for-pull
   (concat user-documents-directory
           (convert-standard-filename "/orgmode/from-mobile.org")))
 '(org-startup-folded (quote content))
 '(package-archives
   (quote
    (("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("e6h" . "http://www.e6h.org/packages/"))))
 '(package-selected-packages
   (quote
    (yaml-mode xlicense wakatime-mode vala-mode sass-mode nyan-mode muse markdown-mode mark magit-gh-pulls magit-gerrit json-mode js2-mode jinja2-mode helm-make helm-gtags helm-flyspell helm-ag go-mode gitignore-mode gitconfig-mode git-gutter ggtags fiplr erlang django-mode company-shell company-quickhelp company-c-headers coffee-mode buffer-move ag)))
 '(safe-local-variable-values
   (quote
    ((company-clang-arguments "-I.." "-I/home/polesz/jhbuild/install/include/atk-1.0" "-I/home/polesz/jhbuild/install/include/at-spi-2.0" "-I/home/polesz/jhbuild/install/include/at-spi2-atk/2.0" "-I/home/polesz/jhbuild/install/include/cairo" "-I/home/polesz/jhbuild/install/include/gdk-pixbuf-2.0" "-I/home/polesz/jhbuild/install/include/gio-unix-2.0/" "-I/home/polesz/jhbuild/install/include/glib-2.0" "-I/home/polesz/jhbuild/install/include/gtk-3.0" "-I/home/polesz/jhbuild/install/include/harfbuzz" "-I/home/polesz/jhbuild/install/include/libgda-5.0" "-I/home/polesz/jhbuild/install/include/libgda-5.0/libgda" "-I/home/polesz/jhbuild/install/include/librsvg-2.0" "-I/home/polesz/jhbuild/install/include/libsoup-2.4" "-I/home/polesz/jhbuild/install/include/pango-1.0" "-I/home/polesz/jhbuild/install/include/swe-glib" "-I/home/polesz/jhbuild/install/include/webkitgtk-4.0" "-I/home/polesz/jhbuild/install/lib/glib-2.0/include" "-I/usr/include/dbus-1.0" "-I/usr/include/freetype2" "-I/usr/include/libdrm" "-I/usr/include/libpng16" "-I/usr/include/libxml2" "-I/usr/include/pixman-1" "-I/usr/lib64/dbus-1.0/include")
     (company-clang-arguments "-I.." "-I/home/polesz/jhbuild/install/include" "-I/home/polesz/jhbuild/install/include/atk-1.0" "-I/home/polesz/jhbuild/install/include/at-spi-2.0" "-I/home/polesz/jhbuild/install/include/at-spi2-atk/2.0" "-I/home/polesz/jhbuild/install/include/cairo" "-I/home/polesz/jhbuild/install/include/gdk-pixbuf-2.0" "-I/home/polesz/jhbuild/install/include/gio-unix-2.0/" "-I/home/polesz/jhbuild/install/include/glib-2.0" "-I/home/polesz/jhbuild/install/include/gtk-3.0" "-I/home/polesz/jhbuild/install/include/harfbuzz" "-I/home/polesz/jhbuild/install/include/libgda-5.0" "-I/home/polesz/jhbuild/install/include/libgda-5.0/libgda" "-I/home/polesz/jhbuild/install/include/librsvg-2.0" "-I/home/polesz/jhbuild/install/include/libsoup-2.4" "-I/home/polesz/jhbuild/install/include/pango-1.0" "-I/home/polesz/jhbuild/install/include/swe-glib" "-I/home/polesz/jhbuild/install/include/webkitgtk-4.0" "-I/home/polesz/jhbuild/install/lib/glib-2.0/include" "-I/usr/include/dbus-1.0" "-I/usr/include/freetype2" "-I/usr/include/libdrm" "-I/usr/include/libpng16" "-I/usr/include/libxml2" "-I/usr/include/pixman-1" "-I/usr/lib64/dbus-1.0/include")
     (company-clang-arguments "-I/home/polesz/jhbuild/install/include" "-I/home/polesz/jhbuild/install/include/atk-1.0" "-I/home/polesz/jhbuild/install/include/at-spi-2.0" "-I/home/polesz/jhbuild/install/include/at-spi2-atk/2.0" "-I/home/polesz/jhbuild/install/include/cairo" "-I/home/polesz/jhbuild/install/include/gdk-pixbuf-2.0" "-I/home/polesz/jhbuild/install/include/gio-unix-2.0/" "-I/home/polesz/jhbuild/install/include/glib-2.0" "-I/home/polesz/jhbuild/install/include/gtk-3.0" "-I/home/polesz/jhbuild/install/include/harfbuzz" "-I/home/polesz/jhbuild/install/include/libgda-5.0" "-I/home/polesz/jhbuild/install/include/libgda-5.0/libgda" "-I/home/polesz/jhbuild/install/include/librsvg-2.0" "-I/home/polesz/jhbuild/install/include/libsoup-2.4" "-I/home/polesz/jhbuild/install/include/pango-1.0" "-I/home/polesz/jhbuild/install/include/swe-glib" "-I/home/polesz/jhbuild/install/include/webkitgtk-4.0" "-I/home/polesz/jhbuild/install/lib/glib-2.0/include" "-I/usr/include/dbus-1.0" "-I/usr/include/freetype2" "-I/usr/include/libdrm" "-I/usr/include/libpng16" "-I/usr/include/libxml2" "-I/usr/include/pixman-1" "-I/usr/lib64/dbus-1.0/include")
     (company-clang-arguments "-I/usr/include/glib-2.0"))))
 '(savehist-mode t)
 '(sgml-basic-offset 4)
 '(show-trailing-whitespace t)
 '(sml/theme (quote respectful))
 '(tab-width 4)
 '(wakatime-api-key "3f97611e-c959-4ce3-a526-bf0241307e17")
 '(wakatime-cli-path "/usr/local/bin/wakatime"))

(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq-default magit-gerrit-remote "gerrit")
(set-face-attribute 'default t :font "Hack-10")
(set-frame-font "Hack-10" nil t)
(setq user-mail-address "gergely@polonkai.eu")
(setq helm-M-x-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit nil :background "gray25"))))
 '(trailing-whitespace ((t (:inherit nil :background "red1"))))
 '(whitespace-line ((t (:inherit nil :background "orange")))))

(add-to-list 'load-path (concat user-emacs-directory "move-line"))
(add-to-list 'load-path (concat user-emacs-directory "gobgen"))

; Nyanyanyanyanya
(add-hook 'after-init-hook 'nyan-mode)
(add-hook 'after-init-hook 'global-wakatime-mode)
(add-hook 'after-init-hook (lambda () (require 'magit-gerrit)))

(add-hook 'after-init-hook (lambda () (require 'move-line)))
(add-hook 'after-init-hook (lambda () (require 'whitespace)))
(add-hook 'after-init-hook (lambda () (require 'rcirc)))
(add-hook 'after-init-hook (lambda () (require 'thingatpt)))
(add-hook 'after-init-hook (lambda () (require 'gobgen)))

(load "gnu-c-header.el")
(load "toggle-window-split.el")
(load "round-number-to-decimals.el")
(load "transpose-windows.el")
(load "zim.el")
(load "clearcase.el")
(load "jekyll.el")
(load "enclose-string.el")

(add-hook 'c-mode-hook
          (lambda ()
            (helm-gtags-mode t)
            (ggtags-mode 1)
            (which-func-mode)
            (flyspell-prog-mode)))
(add-hook 'c-mode-common-hook
          (lambda ()
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

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)

(require 'helm-config)
(helm-mode 1)
(eval-after-load 'company
  '(progn
     (define-key company-mode-map (kbd "C-:") 'helm-company)
     (define-key company-active-map (kbd "C-:") 'helm-company)))
(require 'xlicense)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(global-origami-mode t)
(show-paren-mode t)
(global-set-key (kbd "M-i") 'helm-swoop)

(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(global-git-gutter-mode t)
(git-gutter:linum-setup)

(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(add-hook 'multiple-cursors-mode-enabled-hook
          (lambda ()
            (setq blink-matching-paren nil)))
(add-hook 'multiple-cursors-mode-disabled-hook
          (lambda ()
            (setq blink-matching-paren t)))

(sml/setup)

(add-hook 'org-mode-hook
          (lambda ()
            (if (display-graphic-p) (org-bullets-mode t))))

(add-hook 'text-mode-hook (lambda () (visual-line-mode t)))

(global-set-key (kbd "M-(") 'æ-enclose-region)

;; TODO: maybe add-to-list is a better way to do it
(setq insert-pair-alist
      '(
       (40 41)      ; ()
       (91 93)      ; []
       (123 125)    ; {}
       (60 62)      ; <>
       (34 34)      ; ""
       (39 39)      ; ''
       (96 39)      ; `'
       (8220 8221)  ; “”
       (8222 8221)  ; „”
       (8216 8217)  ; ‘’
       (8249 8250)  ; ‹›
       (8250 8249)  ; ›‹
       (171 187)    ; «»
       (187 171)    ; »«
      ))

;; Other parens might be added later (in no particular order):
; （） ［］ ｛｝ ｟｠
; ⦅⦆ 〚〛 ⦃⦄ 「」 〈〉 《》 【】 〔〕 ⦗⦘
; 『』 〖〗 〘〙
; ｢｣
; ⟦⟧ ⟨⟩ ⟪⟫ ⟮⟯ ⟬⟭ ⌈⌉ ⌊⌋ ⦇⦈ ⦉⦊
; ❛❜ ❝❞ ❨❩ ❪❫ ❴❵ ❬❭ ❮❯ ❰❱ ❲❳
; ﴾﴿
;
; ⏜⏝ ⎴⎵ ⏞⏟ ⏠⏡
; ﹁﹂ ﹃﹄ ︹︺ ︻︼ ︗︘ ︿﹀ ︽︾ ﹇﹈ ︷︸
; 〈〉 ⦑⦒ ⧼⧽
; ﹙﹚ ﹛﹜ ﹝﹞
; ⁽⁾ ₍₎
; ⦋⦌ ⦍⦎ ⦏⦐ ⁅⁆
; ⸢⸣ ⸤⸥
; ⟅⟆ ⦓⦔ ⦕⦖ ⸦⸧ ⸨⸩ ⧘⧙ ⧚⧛; ⸜⸝ ⸌⸍ ⸂⸃ ⸄⸅ ⸉⸊
; ᚛᚜ ༺༻ ༼༽

(global-prettify-symbols-mode t)

; Bind webjump to a key. It’s pretty handy
(global-set-key (kbd "C-x w") 'webjump)

; Turn off scroll bar (that’s why Nyan-cat is here) and the toolbar (I
; don’t really use it)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
