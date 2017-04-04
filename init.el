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
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
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

;; Load my own functions
(load "gnu-c-header")
(load "round-number-to-decimals")
(load "zim")
(load "enclose-string")
(load "buf-manipulation")
(load "text-manip")
(load "frame-manip")
(load "file-manip")
(load "window-manip")

;; Define aliases
(defalias 'yes-or-no-p 'y-or-n-p)

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
  :bind (("C-c m t" . mc/mark-all-like-this)
         ("C-c m m" . mc/mark-all-like-this-dwim)
         ("C-c m l" . mc/edit-lines)
         ("C-c m e" . mc/edit-ends-of-lines)
         ("C-c m a" . mc/edit-beginnings-of-lines)
         ("C-c m n" . mc/mark-next-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
         ("C-c m s" . mc/mark-sgml-tag-pair)
         ("C-c m d" . mc/mark-all-like-this-in-defun)))

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
  :ensure t
  :after
  magit)

(use-package magit-gerrit
  :ensure t
  :after
  magit
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
  (([f9] . smartparens-strict-mode)
   ("C-c s u" . sp-unwrap-sexp)
   ("C-c s k" . sp-kill-sexp)))

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
  (add-to-list 'sml/replacer-regexp-list
               (list "^:Proj:python/" ":Proj:Py:")
               t)
  (sml/setup))

(use-package company
  :ensure t
  :config
  (setq company-idle-delay nil)
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
  (drag-stuff-global-mode t)
  (drag-stuff-define-keys))

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
     ("gg" . git-gutter:update-all-windows)
     ("gn" . git-gutter:next-hunk)
     ("gp" . git-gutter:previous-hunk)))))

;; From gmane.emacs.orgmode
;; (http://article.gmane.org/gmane.emacs.orgmode/75222)
(defun f-ediff-org-showhide (buf command &rest cmdargs)
  "If buffer BUF exists and in org-mode, execute COMMAND with CMDARGS."
  (when buf
    (when (eq (buffer-local-value 'major-mode (get-buffer buf)) 'org-mode)
      (save-excursion
        (set-buffer buf)
        (apply command cmdargs)))))

(defun f-ediff-org-unfold-tree-element ()
  "Unfold tree at diff location."
  (f-ediff-org-showhide ediff-buffer-A 'org-reveal)
  (f-ediff-org-showhide ediff-buffer-B 'org-reveal)
  (f-ediff-org-showhide ediff-buffer-C 'org-reveal))

(defun f-ediff-org-fold-tree ()
  "Fold tree back to top level."
  (f-ediff-org-showhide ediff-buffer-A 'hide-sublevels 1)
  (f-ediff-org-showhide ediff-buffer-B 'hide-sublevels 1)
  (f-ediff-org-showhide ediff-buffer-C 'hide-sublevels 1))

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
  (require 'ox-md)
  (unless (boundp 'org-capture-templates)
    (setq org-capture-templates nil))
  (add-to-list 'org-capture-templates
               '("p" "Blog post"
                 entry (file+datetree (concat org-directory "blog.org"))
                 "* %^{Title}  :blog:\n   :PROPERTIES:\n   :on: %T\n   :END:\n   %i%?"))
  (add-to-list 'org-capture-templates
               '("g" "GT2 note"
                 entry (file+headline (concat org-directory "gt2-notes.org")
                                      "Captures")
                 "** %^{Title}\n   :PROPERTIES:\n   :on: %T\n   :END:\n   %i%?"))
  (setq org-time-stamp-formats '("<%Y-%m-%d>" . "<%Y-%m-%d %H:%M>")
        org-todo-keywords '((sequence "TODO(t)"
                                      "DOING(w@/!)"
                                      "BLOCKED(b@/!)"
                                      "|"
                                      "REVIEW(r@/!)"
                                      "DONE(d@/!)"))
        org-goto-interface 'outline-path-completion
        org-goto-max-level 10
        org-html-checkbox-type 'unicode
        org-html-checkbox-types
        '((unicode (on . "<span class=\"task-done\">☑</span>")
            (off . "<span class=\"task-todo\">☐</span>")
            (trans . "<span class=\"task-in-progress\">▣</span>")))
        org-src-window-setup 'current-window)
  (add-hook 'ediff-select-hook 'f-ediff-org-unfold-tree-element)
  (add-hook 'ediff-unselect-hook 'f-ediff-org-fold-tree)
  :bind
  (:map gpolonkai/pers-map
   ("a" . org-agenda-list)
   ("c" . org-capture)
   :map org-mode-map
   ("SPC" . org-space-key)
   ("C-c l" . org-toggle-link-display)
   ("C-a" . gpolonkai/move-to-beginning-of-line)
   ("C-e" . gpolonkai/move-to-end-of-line)))

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
  :config
  (setq aw-background nil
        aw-dispatch-always t)
  (add-to-list 'aw-dispatch-alist
               '(?s gpolonkai/scroll-window-up " Scroll window up")
               t)
  (add-to-list 'aw-dispatch-alist
               '(?S gpolonkai/scroll-window-down " Scroll window down")
               t)
  (add-to-list 'aw-dispatch-alist
               '(?q gpolonkai/bury-window " Bury (quit) window")
               t)
  :bind
  (:map ctl-x-map
   ("o" . ace-window)))

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
  :pin gnu
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
  :ensure t
  :config
  (load "idm")
  (setq idm-database-file (expand-file-name "idm-db.gpg" user-emacs-directory))
  :bind
  (:map gpolonkai/pers-map
   ("i" . idm-open-list-command)))

(use-package identica-mode
  :ensure t)

(use-package jinja2-mode
  :ensure t)

(use-package js2-mode
  :ensure t
  :pin melpa-stable)

(use-package json-mode
  :ensure t)

(use-package markdown-mode
  :ensure t
  :config
  (push '("\\.markdown\\'" . markdown-mode) auto-mode-alist)
  (push '("\\.md\\'" . markdown-mode) auto-mode-alist))

(use-package mc-extras
  :ensure t
  :demand
  :bind
  (:map mc/keymap
   ("C-c m =" . mc/compare-chars)))

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
  :demand
  :config
  (yas-global-mode 1)
  :bind
  ;; Remove TAB binding to yas-expand.  It causes more harm than good.
  (:map yas-minor-mode-map
   ("TAB" . nil)
   ([(tab)] . nil)))

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
        auto-package-update-delete-old-versions t)
  ;; Let’s do this in after-init-hook, as use-package invocations may modify
  ;; the list of installed packages
  (add-hook 'after-init-hook 'auto-package-update-maybe))

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

(use-package expand-region
  :ensure t
  :bind
  (:map ctl-x-map
   ("*" . er/expand-region)))

(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :ensure t
  :after
  company
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package mediawiki
  :ensure t
  :after
  id-manager
  :config
  (add-to-list 'mediawiki-site-alist
               '("WikEmacs"
                 "http://wikemacs.org/wiki/"
                 (gpolonkai/idm-get-id-for-account "WikEmacs")
                 (gpolonkai/idm-get-password-for-account "WikEmacs"))))

(use-package github-notifier
  :ensure t
  :after
  id-manager
  :config
  (setq github-notifier-token (gpolonkai/idm-get-password-for-account "GitHub"))
  (github-notifier-mode))

(use-package gist
  :ensure t)

(use-package company-web
  :ensure t
  :config
  (require 'company-web-html))

(use-package enlive
  :ensure t)

(use-package po-mode
  :ensure t)

(use-package dashboard
  :ensure t
  :after
  projectile
  :config
  (add-to-list 'dashboard-items '(projects . 5) t)
  (dashboard-setup-startup-hook))

(use-package csharp-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package dockerfile-mode
  :ensure t)

(use-package phi-search
  :ensure t)

(use-package phi-search-mc
  :ensure t
  :config
  (phi-search-mc/setup-keys))

(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'prettify-symbols-alist
                         '("not" . 172))
            (add-to-list 'prettify-symbols-alist
                         '("in" . 8712))))

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
 ("M-q" . sachachua/fill-or-unfill-paragraph)
 ("C-c r" . round-number-at-point-to-decimals)
 :map ctl-x-map
 ("C-y" . duplicate-line)
 ("_" . maximize-window)
 ("C-r" . rename-current-buffer-file)
 ("C-d" . delete-current-buffer-file)
 ("~" . toggle-char-case)
 ("|" . toggle-window-split)
 ("k" . gpolonkai/kill-this-buffer)
 ("M-c" . gpolonkai/kill-this-buffer-delete-this-window)
 ("M-k" . gpolonkai/undo-buffer-kill)
 :map isearch-mode-map
 ("<C-return>" . isearch-exit-other-end)
 ("<S-return>" . isearch-exit-mark-match)
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

;; Kudos goes to http://emacs.stackexchange.com/a/31321/507
(defun isearch-exit-mark-match ()
  "Exit isearch and mark the current match."
  (interactive)
  (isearch-exit)
  (push-mark isearch-other-end)
  (activate-mark))

;; Set up some global minor modes
(global-prettify-symbols-mode t)

;; Enable some functions
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; text-mode settings
(add-hook 'text-mode-hook (lambda () (visual-line-mode t)))

;; Add some symbols to be prettified
(setq prettify-symbols-alist
      '(("lambda" . 955)  ; λ
        ("->" . 8594)     ; →
        ("=>" . 8658)     ; ⇒
        ("map" . 8614)    ; ↦
        ("not" . 172))    ; ¬

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
