;; Initialize the package system
(package-initialize)

;; Add path to my custom lisp functions
(add-to-list 'load-path (concat
                         user-emacs-directory
                         (convert-standard-filename "lisp/")))
;; Load xdg-paths; it is needed to set `org-directory'
(require 'xdg-paths)

;; Custom stuff
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
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279"
     "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223"
     "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa"
     "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e"
     "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365"
     "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6"
     default)))
 '(ediff-merge-split-window-function (quote split-window-horizontally))
 '(ediff-split-window-function (quote split-window-vertically))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(eww-search-prefix "https://www.google.com/?q=")
 '(fiplr-ignored-globs
   (quote
    ((directories
      (".git" ".svn" ".hg" ".bzr"))
     (files
      (".#*" "*.so" "*~")))))
 '(foreground-color "#5c5cff")
 '(global-hl-line-mode t)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(jekyll-directory "~/Projektek/jekyll/gergely.polonkai.eu")
 '(nxml-attribute-indent 4)
 '(nxml-child-indent 2)
 '(nxml-outline-child-indent 4)
 '(nyan-animate-nyancat t)
 '(nyan-wavy-trail t)
 '(org-crypt-key "B0740C4C")
 '(org-default-notes-file
   (concat user-documents-directory
           (convert-standard-filename "/orgmode/notes.org")))
 '(org-directory
   (concat user-documents-directory
           (convert-standard-filename "/orgmode/")))
 '(org-agenda-files
   (concat user-documents-directory
           (convert-standard-filename "/orgmode/agenda_files")))
 '(org-ellipsis "…#")
 '(org-mobile-directory
   (concat user-documents-directory
           (convert-standard-filename "/orgmode/mobile/")))
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
    (ag
     buffer-move
     coffee-mode
     command-log-mode
     company-c-headers
     company-quickhelp
     company-shell
     django-manage
     django-mode
     drag-stuff
     electric-case
     electric-spacing
     emamux
     erlang
     fiplr
     flycheck
     flycheck-pkg-config
     focus
     ggtags
     gh
     git-gutter
     git-messenger
     git-timemachine
     gitconfig
     gitconfig-mode
     github-notifier
     gitignore-mode
     gnome-calendar
     gnugo
     go-mode
     gobgen
     google
     goto-last-change
     helm-ag
     helm-chrome
     helm-company
     helm-flycheck
     helm-flyspell
     helm-google
     helm-gtags
     helm-make
     helm-projectile
     helm-spotify
     helm-swoop
     helm-unicode
     ht
     hyde
     id-manager
     identica-mode
     jinja2-mode
     js2-mode
     json-mode
     logito
     magit-gerrit
     magithub
     mark
     markdown-mode
     marshal
     mc-extras
     multiple-cursors
     muse
     ng2-mode
     nyan-mode
     nyan-prompt
     org-bullets
     org-jekyll
     org-jira
     org-projectile
     origami
     pcache
     projectile-direnv
     queue
     sass-mode
     smart-mode-line-powerline-theme
     smartparens
     spinner
     sx
     typescript-mode
     use-package
     vala-mode
     wakatime-mode
     xlicense
     yaml-mode
     zone-nyan)))
 '(savehist-mode t)
 '(sgml-basic-offset 4)
 '(show-trailing-whitespace t)
 '(sml/theme (quote powerline))
 '(tab-width 4)
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(zone-nyan-hide-progress t))

;; Custom face settings
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit nil :background "gray25"))))
 '(trailing-whitespace ((t (:inherit nil :background "red1"))))
 '(whitespace-line ((t (:inherit nil :background "orange")))))
(set-face-attribute 'default t :font "Hack-10")
(set-frame-font "Hack-10" nil t)

;; Some personal stuff
(setq user-mail-address "gergely@polonkai.eu")

;; Load some custom libraries
(require 'thingatpt)
(require 'xlicense)
(require 'linum)
(require 'origami)

(use-package helm
  :init
  (require 'helm-config)
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  :config
  (helm-mode t)
  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-mini)))

(use-package helm-swoop
  :bind
  (("M-i" . helm-swoop)))

(use-package ggtags
  :config
  (add-hook 'c-mode-hook
            (lambda ()
              (ggtags-mode t)))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode t)))))

(use-package helm-gtags
  :init
  (setq-default helm-gtags-auto-update t
                helm-gtags-ignore-case t
                helm-gtags-path-style 'relative)
  :config
  (add-hook 'c-mode-hook
            (lambda ()
              (helm-gtags-mode t)))
  :bind
  (:map helm-gtags-mode-map
   ("M-t" . helm-gtags-find-tag)
   ("M-r" . helm-gtags-find-rtag)
   ("M-s" . helm-gtags-find-symbol)
   ("M-g M-p" . helm-gtags-parse-file)
   ("C-c <" . helm-gtags-previous-history)
   ("C-c >" . helm-gtags-next-history)
   ("M-," . helm-gtags-pop-stack)))

;; Whitespace mode
;;
;; It is turned on by default, and can be toggled with F10
(use-package whitespace
  :config
  (global-whitespace-mode t)
  :bind
  (([f10] . global-whitespace-mode)))

;; Multiple cursors
(use-package multiple-cursors
  :config
  (add-hook 'multiple-cursors-mode-enabled-hook
            (lambda ()
              (setq blink-matching-paren nil)))
  (add-hook 'multiple-cursors-mode-disabled-hook
            (lambda ()
              (setq blink-matching-paren t)))
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package eshell
  :config
  (add-hook 'eshell-mode-hook
            (lambda () (local-set-key (kbd "C-d") #'eshell-C-d))))

;; Save place
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

;; Nyanyanyanyanya
(use-package nyan-mode
  :config
  (nyan-mode t))

(use-package nyan-prompt
  :config
  (add-hook 'eshell-load-hook 'nyan-prompt-enable))

;; Zone!
(use-package zone
  :config
  (setq zone-programs [zone-nyan])
  (zone-when-idle 60))

;; Magit and friends
(use-package magit
  :init
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind
  (("C-x g" . magit-status)))

(use-package magithub)

(use-package magit-gerrit
  :init
  (setq-default magit-gerrit-remote "gerrit"))

(use-package ag
  :bind
  (("C-x M-a" . ag)
   ("C-x C-M-a" . ag-regexp)))

(use-package smartparens-config
  :ensure smartparens
  :config
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook
            'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook
            'turn-on-smartparens-strict-mode)
  :bind
  (([f9] . smartparens-strict-mode)))

(use-package fiplr
  :config
  (fiplr-clear-cache))

(use-package smart-mode-line
  :config
  (sml/setup))

(use-package company
  :bind
  (:map company-mode-map
   ("C-:" . helm-company)
   :map company-active-map
   ("C-:" . helm-company))
  :config
  (global-company-mode t))

(use-package projectile
  :config
  (projectile-global-mode t))

;; Load my own functions
(load "gnu-c-header.el")
(load "toggle-window-split.el")
(load "round-number-to-decimals.el")
(load "transpose-windows.el")
(load "zim.el")
(load "clearcase.el")
(load "jekyll.el")
(load "enclose-string.el")
(load "buf-manipulation.el")

;; Define aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; Waka-waka
(add-hook 'after-init-hook 'global-wakatime-mode)

;; `c-mode' settings
(add-hook 'c-mode-hook
          (lambda ()
            (which-func-mode)
            (flyspell-prog-mode)))
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)
            (c-set-style "PERSONAL")
            (setq tab-width 4
                  indent-tabs-mode nil)
            (c-toggle-auto-newline 1)))
(add-hook 'c-initialization-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "C-m") 'c-context-line-break)))
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
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
(setq c-offset-alist '((member-init-intro . ++)))

;; Custom key bindings
(global-set-key (kbd "C-x _") 'maximize-window)
;; Some terminals don’t interpret Alt-Up/Down as M-<up/down>.
(global-set-key (kbd "C-c C-y") 'duplicate-line)
(global-set-key (kbd "M-(") 'æ-enclose-region)
(global-set-key (kbd "C-x w") 'webjump)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-d") 'delete-current-buffer-file)
(global-set-key (kbd "C-x ~") 'toggle-char-case)

;; Origami mode keys
(define-key global-map (kbd "C-x C-z") 'origami-mode-map)
(define-prefix-command 'origami-mode-map)
(define-key origami-mode-map (kbd "o") 'origami-open-node)
(define-key origami-mode-map (kbd "O") 'origami-open-node-recursively)
(define-key origami-mode-map (kbd "c") 'origami-close-node)
(define-key origami-mode-map (kbd "C") 'origami-close-node-recursively)
(define-key origami-mode-map (kbd "a") 'origami-toggle-node)
(define-key origami-mode-map (kbd "A") 'origami-recursively-toggle-node)
(define-key origami-mode-map (kbd "R") 'origami-open-all-nodes)
(define-key origami-mode-map (kbd "M") 'origami-close-all-nodes)
(define-key origami-mode-map (kbd "v") 'origami-show-only-node)
(define-key origami-mode-map (kbd "k") 'origami-previous-fold)
(define-key origami-mode-map (kbd "j") 'origami-forward-fold)
(define-key origami-mode-map (kbd "x") 'origami-reset)

;; Set up some global minor modes
(global-origami-mode t)
(show-paren-mode t)
(global-git-gutter-mode t)
(global-prettify-symbols-mode t)
(drag-stuff-global-mode t)

;; Projectile settings
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; Don’t allow tabs to be inserted during indentation
(setq-default indent-tabs-mode nil)

;; Enable some functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; org-mode settings
(add-hook 'org-mode-hook
          (lambda ()
            (if (display-graphic-p) (org-bullets-mode t))))

;; text-mode settings
(add-hook 'text-mode-hook (lambda () (visual-line-mode t)))

;; UI hacks: turn off scroll bar (that’s why Nyan-cat is here) and the
;; toolbar (I don’t really use it)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; Add some symbols to be prettified
(setq prettify-symbols-alist
      '(("lambda" . 955)  ; λ
        ("->" . 8594)     ; →
        ("=>" . 8658)     ; ⇒
        ("map" . 8614)))  ; ↦
;; …and some pairs to complete
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

;; Stuff to do after initialization is done

;; TODO: Unordered stuff
(add-to-list 'auto-mode-alist '("\\.vala\\'" . vala-mode))
(add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

; Temporary show line numbers while in the goto minibuffer. Copied
; from http://whattheemacsd.com/key-bindings.el-01.html
(global-set-key [remap goto-line] 'goto-line-with-feedback)

; TODO git-gutter-mode and linum-mode don’t play together well. This
; is an attempt to make them nice to each other, but it seems futile.
(defun æ-restore-goto-modes (linum-state gitgutter-state)
  (linum-mode -1)
  (git-gutter-mode -1)
  (linum-mode linum-state)
  (git-gutter-mode gitgutter-state))

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line
  number input"

  (interactive)

  (let ((old-linum-mode linum-mode)
        (old-gitgutter-mode git-gutter-mode))
    (unwind-protect
        (progn
          ; Stay safe with git-gutter turned off
          (git-gutter-mode -1)
          ; Turn on linum, before asking for the line number
          (linum-mode 1)
          (goto-line (read-number "Goto line: "))
          (æ-restore-goto-modes old-linum-mode old-gitgutter-mode))
      (æ-restore-goto-modes old-linum-mode old-gitgutter-mode))))
