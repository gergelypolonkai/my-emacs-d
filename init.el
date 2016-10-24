;; Initialize the package system and use-package
(setq load-prefer-newer t)
(require 'package)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Set up my personal keymap early so I can use it in use-package
;; calls
(define-prefix-command 'gpolonkai/pers-map)
(define-key ctl-x-map "t" 'gpolonkai/pers-map)

;; Add path to my custom lisp functions
(add-to-list 'load-path (concat
                         user-emacs-directory
                         (convert-standard-filename "lisp/")))

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
 '(echo-keystrokes 0.1)
 '(foreground-color "#5c5cff")
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(package-selected-packages
   (quote
    (ace-window
     ag
     alert
     avy
     beacon
     cheatsheet
     coffee-mode
     command-log-mode
     company
     company-c-headers
     company-emoji
     company-restclient
     company-shell
     diminish
     drag-stuff
     electric-case
     electric-spacing
     emamux
     flycheck
     flycheck-pkg-config
     focus
     ggtags
     git-gutter
     git-messenger
     git-timemachine
     gitconfig-mode
     gitignore-mode
     gnugo
     gobgen
     google
     goto-last-change
     helm
     helm-ag
     helm-chrome
     helm-company
     helm-descbinds
     helm-describe-modes
     helm-flycheck
     helm-flyspell
     helm-github-stars
     helm-google
     helm-gtags
     helm-projectile
     helm-smex
     helm-swoop
     helm-unicode
     hungarian-holidays
     hungry-delete
     hyde
     id-manager
     identica-mode
     jinja2-mode
     js2-mode
     json-mode
     magit
     magit-gerrit
     magithub
     markdown-mode
     mc-extras
     multiple-cursors
     ng2-mode
     nyan-mode
     nyan-prompt
     org
     org-bullets
     org-jekyll
     org-projectile
     org-random-todo
     org-rtm
     origami
     plantuml-mode
     projectile
     rainbow-delimiters
     rainbow-mode
     restclient
     restclient-helm
     sass-mode
     simple-rtm
     smart-mode-line
     smart-mode-line-powerline-theme
     smartparens
     spinner
     sx
     typescript-mode
     use-package
     vala-mode
     vala-snippets
     wakatime-mode
     xlicense
     yaml-mode
     yasnippet
     zone-nyan)))
 '(savehist-mode t)
 '(sgml-basic-offset 4)
 '(show-trailing-whitespace t)
 '(tab-width 4))

;; Custom face settings
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0))))
 '(hl-line ((t (:inherit nil :background "gray25"))))
 '(trailing-whitespace ((t (:inherit nil :background "red1"))))
 '(whitespace-line ((t (:inherit nil :background "orange")))))
(set-face-attribute 'default t :font "Hack-10")
(set-frame-font "Hack-10" nil t)

;; Some personal stuff
(setq user-mail-address "gergely@polonkai.eu")

;; Load some custom libraries
(require 'thingatpt)
(require 'linum)

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
  :ensure t
  :demand
  :config
  (global-whitespace-mode 1)
  :bind
  (([f10] . whitespace-mode)
   ([(shift f10)] . global-whitespace-mode)))

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
  :init
  (setq-default nyan-animate-nyancat t
                nyan-wavy-trail t)
  :config
  (nyan-mode t))

(when (display-graphic-p)
  (use-package nyan-prompt
    :config
    (add-hook 'eshell-load-hook 'nyan-prompt-enable)))

;; Zone!
(use-package zone-nyan
  :init
  (setq zone-programs [zone-nyan])
  (setq-default zone-nyan-hide-progress t))

(use-package zone
  :config
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

(use-package origami
  :ensure t
  :demand
  :config
  (define-prefix-command 'origami-mode-map)
  (global-set-key (kbd "C-x C-z") 'origami-mode-map)
  (global-origami-mode)
  :bind
  (:map origami-mode-map
   ("o" . origami-open-node)
   ("O" . origami-open-node-recursively)
   ("c" . origami-close-node)
   ("C" . origami-close-node-recursively)
   ("a" . origami-toggle-node)
   ("A" . origami-recursively-toggle-node)
   ("R" . origami-open-all-nodes)
   ("M" . origami-close-all-nodes)
   ("v" . origami-show-only-node)
   ("k" . origami-previous-fold)
   ("j" . origami-forward-fold)
   ("x" . origami-reset)))

(use-package helm-ag
  :bind
  (("C-x M-a" . helm-do-ag)))

(use-package smartparens
  :ensure t
  :demand
  :config
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook
            'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook
            'turn-on-smartparens-strict-mode)
  :bind
  (([f9] . smartparens-strict-mode)))

(use-package smart-mode-line
  :config
  (sml/setup))

(use-package company
  :config
  (global-company-mode))

(use-package helm-company
  :after
  company
  helm
  :bind
  (:map company-mode-map
   ("C-S-j" . helm-company)
   :map company-active-map
   ("C-S-j" . helm-company)))

(use-package projectile
  :config
  (projectile-global-mode t))

(use-package helm-projectile
  :init
  (setq projectile-completion-system 'helm)
  :config
  (helm-projectile-on))

(use-package drag-stuff
  :config
  (drag-stuff-global-mode t))

;; Git gutter
(use-package git-gutter
  :config
  (global-git-gutter-mode t))

;; Org mode
(use-package org
  :ensure t
  :demand
  :init
  (require 'xdg-paths)
  (setq-default org-crypt-key "B0740C4C"
                org-default-notes-file (concat user-documents-directory
                                               (convert-standard-filename
                                                "/orgmode/notes.org"))
                org-directory (concat user-documents-directory
                                      (convert-standard-filename "/orgmode/"))
                org-agenda-files (concat user-documents-directory
                                         (convert-standard-filename
                                          "/orgmode/agenda_files"))
                org-ellipsis "…#"
                org-startup-folded 'content
                org-mobile-directory (concat user-documents-directory
                                             (convert-standard-filename
                                              "/orgmode/mobile/"))
                org-mobile-inbox-for-pull (concat
                                           user-documents-directory
                                           (convert-standard-filename
                                            "/orgmode/from-mobile.org"))
                org-log-done 'time)
  :config
  (unless (boundp 'org-capture-templates)
    (setq org-capture-templates nil))
  (add-to-list 'org-capture-templates
               '("p" "Blog post"
                 entry (file+datetree (concat org-directory "blog.org"))
                 "* %^{Title}  :blog:\n   :PROPERTIES:\n   :on: %T\n   :END:\n   %i%?"))
  (setq org-time-stamp-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>"))
  :bind
  (:map gpolonkai/pers-map
   ("a" . org-agenda-list)
   :map org-mode-map
   ("SPC" . org-space-key)))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (if (display-graphic-p) org-bullets-mode))))

;; Waka-waka
(use-package wakatime-mode
  :init
  (setq-default wakatime-cli-path "/usr/local/bin/wakatime")
  :config
  (global-wakatime-mode t))

(use-package ace-window
  :init
  (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n)
        aw-background nil)
  :bind
  (("M-P" . ace-window)))

(use-package avy
  :demand
  :config
  (avy-setup-default)
  :bind
  (("C-:" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)))

(use-package diminish
  :defer t)

(use-package focus
  :bind
  (([f8] . focus-mode)))

(use-package coffee-mode)

(use-package command-log-mode)

(use-package company-c-headers)

(use-package company-shell)

(use-package electric-case
  :config
  (add-hook 'c-mode-hook ''electric-case-c-init))

(use-package electric-spacing
  :bind
  (([f7] . electric-spacing-mode)))

(use-package emamux)

(use-package flycheck)

(use-package flycheck-pkg-config)

(use-package git-messenger)

(use-package git-timemachine
  :bind
  (([f6] . git-timemachine-toggle)))

(use-package gitconfig-mode)

(use-package gitignore-mode)

(use-package gnugo)

(use-package gobgen)

(use-package goto-last-change
  :bind
  (("M-g /" . goto-last-change)))

(use-package helm-chrome)

(use-package helm-flycheck)

(use-package helm-flyspell
  :demand
  :bind
  (:map flyspell-mode-map
   ("C-M-i" . helm-flyspell-correct)))

(use-package helm-github-stars
  :init
  (setq-default helm-github-stars-username "gergelypolonkai"))

(use-package helm-google)

(use-package hyde)

(use-package id-manager)

(use-package identica-mode)

(use-package jinja2-mode)

(use-package js2-mode)

(use-package json-mode)

(use-package markdown-mode)

(use-package mc-extras)

(use-package ng2-mode)

(use-package org-projectile)

(use-package sass-mode)

(use-package smart-mode-line-powerline-theme
  :init
  (setq-default sml/theme 'powerline))

(use-package spinner)

(use-package sx)

(use-package typescript-mode)

(use-package vala-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.vala\\'" . vala-mode)))

(use-package xlicense)

(use-package yaml-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package helm-smex
  :bind
  (("M-X" . helm-smex)))

(use-package ediff
  :init
  (setq-default ediff-merge-split-window-function 'split-window-horizontally
                ediff-split-window-function 'split-window-vertically
                ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package plantuml-mode
  :init
  (setq plantuml-jar-path
        (expand-file-name "~/Downloads/plantuml.jar"))
  (defvaralias 'org-plantuml-jar-path 'plantuml-jar-path)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t))))

(use-package org-random-todo
  :config
  ;; Don’t bug me too often…
  (setq org-random-todo-how-often 3600))

(use-package calendar
  :init
  (setq calendar-week-start-day 1
        calendar-latitude 47.4
        calendar-longitude 19.0
        calendar-location-name "Budapest, Hungary"
        calendar-time-zone 60
        calendar-standard-time-zone-name "CET"
        calendar-daylight-time-zone-name "CEST"))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package vala-snippets
  :after
  yasnippet)

(use-package hungarian-holidays
  :config
  (hungarian-holidays-add))

(use-package beacon
  :demand
  :config
  (beacon-mode 1)
  :bind
  (:map gpolonkai/pers-map
   ("b" . beacon-blink)))

(use-package flyspell
  :config
  (add-hook 'prog-mode-hook
            'flyspell-prog-mode)
  (add-hook 'text-mode-hook
            'flyspell-mode))

(use-package helm-descbinds)

(use-package helm-describe-modes)

(use-package paren
  :config
  (show-paren-mode t))

(use-package autorevert
  :config
  (global-auto-revert-mode 1))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook
            #'rainbow-delimiters-mode))

(use-package rainbow-mode)

(use-package hungry-delete
  :config
  (global-hungry-delete-mode))

(use-package hl-line
  :config
  (global-hl-line-mode))

(use-package eww
  :config
  (setq eww-search-prefix "https://www.google.com/?q="))

(use-package electric
  :config
  ;; This seems to be the default, but let’s make sure…
  (electric-indent-mode 1))

(use-package restclient)

(use-package company-restclient)

(use-package restclient-helm)

(use-package alert
  :config
  (setq alert-default-style 'notifications))

(use-package newsticker
  :config
  (setq newsticker-url-list '(("(or emacs irrelevant)"
                               "http://oremacs.com/atom.xml"
                               nil nil nil)
                              ("think"
                               "http://batsov.com/atom.xml"
                               nil nil nil)
                              ("Endless Parentheses"
                               "http://endlessparentheses.com/atom.xml"
                               nil inl nil)))
  :bind
  (:map gpolonkai/pers-map
   ("n" . newsticker-show-news)))

(use-package nxml
  :config
  (setq nxml-attribute-indent 4
        nxml-child-indent 2
        nxml-outline-child-indent 4))

;; Load my own functions
(load "gnu-c-header.el")
(load "toggle-window-split.el")
(load "round-number-to-decimals.el")
(load "transpose-windows.el")
(load "zim.el")
(load "clearcase.el")
(load "enclose-string.el")
(load "buf-manipulation.el")
(load "package-manip")
(load "text-manip")

;; Define aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; `c-mode' settings
(add-hook 'c-mode-hook
          (lambda ()
            (which-func-mode)))
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
(global-set-key (kbd "C-c C-y") 'duplicate-line)
(global-set-key (kbd "M-(") 'æ-enclose-region)
(global-set-key (kbd "C-x w") 'webjump)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-d") 'delete-current-buffer-file)
(global-set-key (kbd "C-x ~") 'toggle-char-case)
(define-key isearch-mode-map (kbd "<C-return>")
  #'isearch-exit-other-end)

;; Kudos goes to
;; http://endlessparentheses.com/leave-the-cursor-at-start-of-match-after-isearch.html
(defun isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string"
  (interactive)

  (isearch-exit)
  (goto-char isearch-other-end))

;; Set up some global minor modes
(global-prettify-symbols-mode t)

;; Enable some functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

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

;; Check for package upgrades every Monday so I don’t forget.
(check-for-package-upgrades-on-day 1)
