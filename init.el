(setq custom-file (concat user-emacs-directory "customizations.el"))
(load custom-file)

;; Initialize the package system and use-package
(setq load-prefer-newer t)
(require 'package)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Set up my personal keymap early so I can use it in use-package
;; calls
(defvar gpolonkai/pers-map (make-sparse-keymap)
  "My own, personal, keymap!")
(define-prefix-command 'gpolonkai/pers-map)
(define-key ctl-x-map "t" 'gpolonkai/pers-map)

;; Add path to my custom lisp functions
(add-to-list 'load-path (concat
                         user-emacs-directory
                         (convert-standard-filename "lisp/")))

(set-face-attribute 'default t :font "Hack-10")
(set-frame-font "Hack-10" nil t)

;; UI hacks: turn off the scroll bar (that’s why Nyan-cat is here),
;; the toolbar (I don’t really use it), and the menu bar (I rarely use
;; it, and in those rare occasions I can simply turn it on)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; Maximize the frame
(set-frame-parameter nil 'fullscreen 'maximized)

;; Some personal stuff
(setq user-mail-address "gergely@polonkai.eu")

;; Load some custom libraries
(require 'thingatpt)

(use-package helm
  :ensure t
  :init
  (require 'helm-config)
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)
  :config
  (helm-mode t)
  :bind
  (("M-x" . helm-M-x)
   :map ctl-x-map
   ("C-f" . helm-find-files)
   ("b" . helm-mini)))

(use-package helm-swoop
  :ensure t
  :bind
  (("M-i" . helm-swoop)))

(use-package ggtags
  :ensure t
  :config
  (add-hook 'c-mode-hook
            (lambda ()
              (ggtags-mode t)))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                (ggtags-mode t)))))

(use-package helm-gtags
  :ensure t
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
  :demand
  :config
  (global-whitespace-mode 1)
  :bind
  (([f10] . whitespace-mode)
   ([(shift f10)] . global-whitespace-mode)))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
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
            (lambda () (local-set-key (kbd "C-d") #'eshell-C-d)))
  :bind
  (:map gpolonkai/pers-map
   ("e" . eshell)))

;; Save place
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

;; Nyanyanyanyanya
(use-package nyan-mode
  :ensure t
  :init
  (setq-default nyan-animate-nyancat t
                nyan-wavy-trail t)
  :config
  (nyan-mode t))

(when (display-graphic-p)
  (use-package nyan-prompt
    :ensure t
    :config
    (add-hook 'eshell-load-hook 'nyan-prompt-enable)))

;; Zone!
(when (display-graphic-p)
  (use-package zone-nyan
    :ensure t
    :after
    zone
    :config
    (setq-default zone-nyan-hide-progress t)
    (setq zone-programs (vconcat zone-programs [zone-nyan]))))

(defun gpolonkai/zone-enable ()
  (interactive)
  (zone-when-idle 60)
  (message "I will zone out after idling for 60 seconds."))

(use-package zone
  :demand
  :config
  (zone-when-idle 60)
  :bind
  (:map gpolonkai/pers-map
   ("zi" . gpolonkai/zone-enable)
   ("zq" . zone-leave-me-alone)))

;; Magit and friends
(use-package magit
  :ensure t
  :init
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind
  (:map ctl-x-map
   ("g" . magit-status)))

(use-package magithub
  :ensure t)

(use-package magit-gerrit
  :ensure t
  :init
  (setq-default magit-gerrit-remote "gerrit"))

(use-package origami
  :ensure t
  :demand
  :config
  (define-prefix-command 'origami-mode-map)
  (define-key ctl-x-map (kbd "C-z") 'origami-mode-map)
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
  :ensure t
  :bind
  (:map gpolonkai/pers-map
   ("s" . helm-do-ag)))

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
  :ensure t
  :after
  org
  :config
  (add-to-list 'sml/replacer-regexp-list
               '("^~/Projects/" ":Proj:")
               t)
  (add-to-list 'sml/replacer-regexp-list
               '("^~/Projektek/" ":Proj:")
               t)
  (add-to-list 'sml/replacer-regexp-list
               (list (concat "^" (regexp-quote user-documents-directory))
                     ":Doc:")
               t)
  (add-to-list 'sml/replacer-regexp-list
               '("^:Proj:jekyll/gergelypolonkai.github.io/" ":Blog:")
               t)
  (sml/setup))

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package helm-company
  :ensure t
  :after
  company
  helm
  :bind
  (:map company-mode-map
   ("C-S-j" . helm-company)
   :map company-active-map
   ("C-S-j" . helm-company)))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t))

(use-package helm-projectile
  :ensure t
  :init
  (setq projectile-completion-system 'helm)
  :config
  (helm-projectile-on))

(use-package drag-stuff
  :ensure t
  :config
  (drag-stuff-global-mode t))

;; Git gutter
;; If we are in text-only mode, there is no fringe.
(let ((gitgutter-package
       (if (display-graphic-p)
           "git-gutter-fringe"
         "git-gutter")))
  (eval `(use-package ,gitgutter-package
    :ensure t
    :demand
    :config
    (global-git-gutter-mode t)
    :bind
    (:map gpolonkai/pers-map
     ("gg" . git-gutter:update-all-windows)))))

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
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(w@/!)" "BLOCKED(b@/!)" "|" "REVIEW(r@/!)" "DONE(d@/!)")))
  :bind
  (:map gpolonkai/pers-map
   ("a" . org-agenda-list)
   ("c" . org-capture)
   :map org-mode-map
   ("SPC" . org-space-key)
   ("C-c l" . org-toggle-link-display)))

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (if (display-graphic-p) org-bullets-mode))))

;; Waka-waka
(use-package wakatime-mode
  :ensure t
  :init
  (setq-default wakatime-cli-path "/usr/bin/wakatime")
  :config
  (global-wakatime-mode t))

(use-package ace-window
  :ensure t
  :init
  (setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n)
        aw-background nil)
  :bind
  (("M-P" . ace-window)))

(use-package avy
  :ensure t
  :demand
  :config
  (avy-setup-default)
  :bind
  (("M-g c" . avy-goto-char)
   ("C-'" . avy-goto-char-2)
   ("M-g f" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g e" . avy-goto-word-0)))

(use-package diminish
  :ensure t
  :defer t)

(use-package focus
  :ensure t
  :bind
  (([f8] . focus-mode)))

(use-package coffee-mode
  :ensure t)

(use-package command-log-mode
  :ensure t)

(use-package company-c-headers
  :ensure t)

(use-package company-shell
  :ensure t)

(use-package electric-case
  :ensure t
  :config
  (add-hook 'c-mode-hook 'electric-case-c-init))

(use-package electric-spacing
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'electric-spacing-mode))

(use-package emamux
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package flycheck-pkg-config
  :ensure t)

(use-package git-messenger
  :ensure t
  :bind
  (:map gpolonkai/pers-map
   ("gm" . git-messenger:popup-message)))

(use-package git-timemachine
  :ensure t
  :bind
  (([f6] . git-timemachine-toggle)))

(use-package gitconfig-mode
  :ensure t)

(use-package gitignore-mode
  :ensure t)

(use-package gnugo
  :ensure t)

(use-package gobgen
  :ensure t)

(use-package goto-last-change
  :ensure t
  :bind
  (("M-g /" . goto-last-change)))

(use-package helm-chrome
  :ensure t)

(use-package helm-flycheck
  :ensure t)

(use-package helm-flyspell
  :ensure t
  :demand
  :bind
  (:map flyspell-mode-map
   ("C-M-i" . helm-flyspell-correct)))

(use-package helm-github-stars
  :ensure t
  :init
  (setq-default helm-github-stars-username "gergelypolonkai"))

(use-package helm-google
  :ensure t)

(use-package hyde
  :ensure t)

(use-package id-manager
  :ensure t)

(use-package identica-mode
  :ensure t)

(use-package jinja2-mode
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :config
  (push '("\\.markdown\\'" . markdown-mode) auto-mode-alist)
  (push '("\\.md\\'" . markdown-mode) auto-mode-alist))

(use-package mc-extras
  :ensure t)

(use-package ng2-mode
  :ensure t)

(use-package org-projectile
  :ensure t)

(use-package sass-mode
  :ensure t)

(use-package smart-mode-line-powerline-theme
  :ensure t
  :init
  (setq-default sml/theme 'powerline))

(use-package spinner
  :ensure t)

(use-package sx
  :ensure t
  :demand
  :bind
  (:map gpolonkai/pers-map
   ("qi" . sx-inbox)
   ("qs" . sx-search)))

(use-package typescript-mode
  :ensure t)

(use-package vala-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.vala\\'" . vala-mode)))

(use-package xlicense
  :ensure t)

(use-package yaml-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode)))

(use-package helm-smex
  :ensure t
  :bind
  (("M-S-x" . helm-smex)))

(use-package ediff
  :ensure t
  :init
  (setq-default ediff-merge-split-window-function 'split-window-horizontally
                ediff-split-window-function 'split-window-vertically
                ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package plantuml-mode
  :ensure t
  :init
  (setq plantuml-jar-path
        (expand-file-name
         ;; Make sure we have a download location even if XDG is not
         ;; working
         (cond
          ((xdg-user-dir "DOWNLOAD")
           (concat (xdg-user-dir "DOWNLOAD") "/plantuml.jar"))
          (t
           "~/Downloads/plantuml.jar"))))
  (defvaralias 'org-plantuml-jar-path 'plantuml-jar-path)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t))))

(use-package org-random-todo
  :ensure t
  :demand
  :config
  ;; Don’t bug me too often…
  (setq org-random-todo-how-often 3600)
  :bind
  (:map gpolonkai/pers-map
   ("r" . org-random-todo)))

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
  :ensure t
  :config
  (yas-global-mode 1))

(use-package vala-snippets
  :ensure t
  :after
  yasnippet)

(use-package hungarian-holidays
  :ensure t
  :config
  (hungarian-holidays-add))

(use-package beacon
  :ensure t
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

(use-package helm-descbinds
  :ensure t)

(use-package helm-describe-modes
  :ensure t)

(use-package paren
  :config
  (show-paren-mode t))

(use-package autorevert
  :config
  (global-auto-revert-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook
            #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t)

(use-package hungry-delete
  :ensure t
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

(use-package restclient
  :ensure t)

(use-package company-restclient
  :ensure t)

(use-package restclient-helm
  :ensure t)

(use-package alert
  :ensure t
  :config
  (setq alert-default-style 'notifications))

(use-package newsticker
  :demand
  :config
  (setq newsticker-url-list '(("(or emacs irrelevant)"
                               "http://oremacs.com/atom.xml"
                               nil nil nil)
                              ("think"
                               "http://batsov.com/atom.xml"
                               nil nil nil)
                              ("Endless Parentheses"
                               "http://endlessparentheses.com/atom.xml"
                               nil nil nil)
                              ("Irreal"
                               "http://irreal.org/blog/?feed=rss2"
                               nil nil nil)
                              ;; The followint may supersede previous entries
                              ("Planet Emacs"
                               "http://planet.emacsen.org/atom.xml"
                               nil nil nil)))
  :bind
  (:map gpolonkai/pers-map
   ("n" . newsticker-show-news)))

(use-package cheatsheet
  :ensure t)

(use-package nxml-mode
  :config
  (setq nxml-attribute-indent 4
        nxml-child-indent 2
        nxml-outline-child-indent 4))

(use-package savehist
  :config
  (savehist-mode 1))

;; Before this can be used, make sure the Symbola font is installed:
;; https://zhm.github.io/symbola/
(defun --set-emoji-font (frame)
  "Adjust the font setting of FRAME so Emacs can display Emoji properly."
  (when (fboundp 'set-fontset-font)
    (if (eq system-type 'darwin)
        ;; For NS/Cocoa
        (set-fontset-font t 'symbol
                          (font-spec :family "Apple Color Emoji")
                          frame 'prepend)
      ;; For Linux
      (set-fontset-font t 'symbol
                       (font-spec :family "Symbola")
                       frame 'prepend))))

(use-package company-emoji
  :ensure t
  :after
  company
  :init
  (--set-emoji-font nil)
  :config
  (add-to-list 'company-backends 'company-emoji)
  (add-hook 'after-make-frame-functions
            '--set-emoji-font))

(use-package zygospore
  :ensure t
  :bind
  (:map ctl-x-map
   ("1" . zygospore-toggle-delete-other-windows)))

(use-package webjump
  :bind
  (:map gpolonkai/pers-map
   ("j" . webjump)))

(use-package which-func
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (which-func-mode)))
  (setq which-func-unknown "∅"))

(use-package ace-popup-menu
  :ensure t
  :config
  (ace-popup-menu-mode 1))

(use-package bind-key
  :ensure t)

(use-package kanban
  :ensure t)

(use-package achievements
  :ensure t
  :config
  (achievements-mode 1))

(use-package auto-highlight-symbol
  :ensure t
  :config
  (global-auto-highlight-symbol-mode t))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-interval 7
        auto-package-update-delete-old-versions t))

(use-package cookie1
  :demand t
  :config
  (setq cookie-file (concat user-emacs-directory "fortune-cookies.txt"))
  :bind
  (:map gpolonkai/pers-map
   ("k" . cookie)))

(use-package ace-mc
  :ensure t)

(use-package dired-k
  :ensure t
  :bind
  (:map dired-mode-map
   ("K" . dired-k)))

(use-package fill-column-indicator
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'fci-mode))

(use-package form-feed
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'form-feed-mode))

(use-package anzu
  :ensure t
  :config
  (global-anzu-mode 1))

(use-package auto-virtualenv
  :ensure t
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'projectile-after-switch-project-hook
            'auto-virtualenv-set-virtualenv))

(use-package flymake-python-pyflakes
  :ensure t)

(use-package gitlab
  :ensure t)

(use-package files
  :config
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

(use-package easy-kill
  :ensure t
  :demand t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package helm-c-yasnippet
  :ensure t
  :demand t
  :config
  (setq helm-yas-space-match-any-greedy t)
  :bind
  (("C-c y" . helm-yas-complete)))

(use-package helm-hunks
  :ensure t)

(use-package helm-pydoc
  :ensure t)

(use-package hl-todo
  :ensure t)

(use-package glasses
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'glasses-mode))

(when (display-graphic-p)
  (use-package eshell-fringe-status
    :ensure t
    :config
    (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode)))

(use-package eshell-prompt-extras
  :ensure t
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda)))

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

;; Load my own functions
(load "gnu-c-header.el")
(load "toggle-window-split.el")
(load "round-number-to-decimals.el")
(load "transpose-windows.el")
(load "zim.el")
(load "enclose-string.el")
(load "buf-manipulation.el")
(load "text-manip")
(load "frame-manip")
(load "file-manip")

;; Define aliases
(defalias 'yes-or-no-p 'y-or-n-p)

;; `c-mode' settings
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
(bind-keys
 :map global-map
 ("M-(" . æ-enclose-region)
 ("<C-return>" . open-line-below)
 ("<C-S-return>" . open-line-above)
 ("M-t" . nil) ;; Remove the old keybinding
 ("M-t c" . transpose-chars)
 ("M-t w" . transpose-words)
 ("M-t l" . transpose-lines)
 ("M-t e" . transpose-sexps)
 ("M-t s" . transpose-sentences)
 ("M-t p" . transpose-paragraphs)
 ("M-t W" . transpose-windows)
 ("C-a" . gpolonkai/move-to-beginning-of-line)
 ("C-e" . gpolonkai/move-to-end-of-line)
 :map ctl-x-map
 ("C-y" . duplicate-line)
 ("_" . maximize-window)
 ("C-r" . rename-current-buffer-file)
 ("C-d" . delete-current-buffer-file)
 ("~" . toggle-char-case)
 :map isearch-mode-map
 ("<C-return>" . isearch-exit-other-end)
 :map gpolonkai/pers-map
 ("m" . hidden-mode-line-mode)
 ("C-i e" . "gergely@polonkai.eu")
 ("C-i w" . "http://gergely.polonkai.eu/")
 ("C-p" . package-list-packages)
 ("o i" . gpolonkai/visit-init-file)
 ("o o" . gpolonkai/visit-org-index))

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

;; Add some symbols to be prettified
(setq prettify-symbols-alist
      '(("lambda" . 955)  ; λ
        ("->" . 8594)     ; →
        ("=>" . 8658)     ; ⇒
        ("map" . 8614))   ; ↦

      ;; …and some pairs to complete
      ;; TODO: maybe add-to-list is a better way to do it
      insert-pair-alist '(
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
                          (187 171))   ; »«

      ;; Set the frame title to the current file name
      frame-title-format '((:eval (concat system-name
                                          ": "
                                          (if (buffer-file-name)
                                              (abbreviate-file-name
                                               (buffer-file-name))
                                            "%b")))))
