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

(setq use-package-always-ensure t)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package use-package
  :pin melpa-stable)

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

(defun termux-p ()
  "Check if Emacs is running under Termux."
  (string-match-p
   (regexp-quote "/com.termux/")
   (expand-file-name "~")))

;; Load some custom libraries
(use-package thingatpt
  :ensure nil)

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
   :map ctl-x-map
   ("C-f" . helm-find-files)
   ("b" . helm-mini)
   :map helm-map
   ("/" . gpolonkai/helm-ff-slash-dir-complete)))

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
  :demand
  :config
  (global-whitespace-mode 1)
  (setq whitespace-line-column 100)
  :bind
  (([f10] . whitespace-mode)
   ([(shift f10)] . global-whitespace-mode)))

;; Multiple cursors
(use-package multiple-cursors
  :init
  (defvar gpolonkai/mc-prefix-map (make-sparse-keymap)
    "Prefix keymap for multiple-cursors")
  (define-prefix-command 'gpolonkai/mc-prefix-map)
  (define-key global-map (kbd "C-c m") 'gpolonkai/mc-prefix-map)
  :config
  (add-hook 'multiple-cursors-mode-enabled-hook
            (lambda ()
              (setq blink-matching-paren nil)))
  (add-hook 'multiple-cursors-mode-disabled-hook
            (lambda ()
              (setq blink-matching-paren t)))
  :bind (:map gpolonkai/mc-prefix-map
         ("t" . mc/mark-all-like-this)
         ("m" . mc/mark-all-like-this-dwim)
         ("l" . mc/edit-lines)
         ("e" . mc/edit-ends-of-lines)
         ("a" . mc/edit-beginnings-of-lines)
         ("n" . mc/mark-next-like-this)
         ("p" . mc/mark-previous-like-this)
         ("s" . mc/mark-sgml-tag-pair)
         ("d" . mc/mark-all-like-this-in-defun)))

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
  :init
  (setq-default nyan-animate-nyancat t
                nyan-wavy-trail t)
  :config
  (nyan-mode t))

(when (display-graphic-p)
  (use-package nyan-prompt
    :disabled t
    :config
    (add-hook 'eshell-load-hook 'nyan-prompt-enable)))

;; Zone!
(when (display-graphic-p)
  (use-package zone-nyan
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
  :init
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind
  (:map ctl-x-map
   ("g" . magit-status)))

(use-package origami
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
  :bind
  (:map gpolonkai/pers-map
   ("s" . helm-do-ag)))

(use-package smartparens
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
  (sml/setup)
  (setq-default header-line-format
                '(""
                  mode-line-mule-info
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification
                  sml/pos-id-separator
                  (vc-mode vc-mode)
                  mode-line-position))
  (delete '(vc-mode vc-mode) mode-line-format)
  (delete 'mode-line-frame-identification mode-line-format)
  (delete 'mode-line-buffer-identification mode-line-format)
  (delete 'mode-line-position mode-line-format)
  (delete 'mode-line-mule-info mode-line-format)
  (delete 'mode-line-modified mode-line-format)
  (delete 'mode-line-client mode-line-format)
  (delete 'mode-line-remote mode-line-format)
  ;; Add sml/pre-id-separator after mode-line-front-space
  (let* ((front-space-position (1+ (cl-position 'mode-line-front-space mode-line-format)))
         (mode-line-rest (nthcdr front-space-position mode-line-format))
         (mode-line-beg (subseq mode-line-format 0 front-space-position)))
    (setq-default mode-line-format (nconc mode-line-beg
                                          (list sml/pre-id-separator)
                                          mode-line-rest)))
)

(use-package company
  :config
  (setq company-idle-delay nil)
  (global-company-mode))

(use-package helm-company
  :after
  company
  helm
  :bind
  (:map company-mode-map
   ("C-c j" . helm-company)
   :map company-active-map
   ("C-c j" . helm-company)))

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
  (drag-stuff-global-mode t)
  (drag-stuff-define-keys))

;; Git gutter
;; If we are in text-only mode, there is no fringe.
(let ((gitgutter-package
       (if (display-graphic-p)
           "git-gutter-fringe"
         "git-gutter")))
  (eval `(use-package ,gitgutter-package
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
                org-log-done 'time
                org-src-preserve-indentation t)
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
                                      "CANCELED(c@/!)"
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
   ("l" . org-store-link)
   :map org-mode-map
   ("SPC" . org-space-key)
   ("C-c l" . org-toggle-link-display)
   ("C-a" . gpolonkai/move-to-beginning-of-line)
   ("C-e" . gpolonkai/move-to-end-of-line)))

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook
            (lambda ()
              (if (display-graphic-p) org-bullets-mode))))

;; Waka-waka
(use-package wakatime-mode
  :init
  (setq-default wakatime-cli-path (executable-find "wakatime"))
  :config
  (global-wakatime-mode t))

(use-package ace-window
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
  (add-hook 'c-mode-hook 'electric-case-c-init))

(use-package electric-operator
  :config
  (add-hook 'c-mode-common-hook 'electric-operator-mode)
  ;; Apply electric-operator-mode to vala-mode, too
  (apply #'electric-operator-add-rules-for-mode 'vala-mode
         electric-operator-prog-mode-rules))

(use-package emamux)

(use-package flycheck)

(use-package flycheck-pkg-config)

(use-package git-messenger
  :bind
  (:map gpolonkai/pers-map
   ("gm" . git-messenger:popup-message)))

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

(use-package id-manager
  :config
  (load "idm")
  (setq idm-database-file (expand-file-name "idm-db.gpg" user-emacs-directory))
  :bind
  (:map gpolonkai/pers-map
   ("i" . idm-open-list-command)))

(use-package identica-mode)

(use-package jinja2-mode)

(use-package js2-mode
  :pin melpa-stable)

(use-package json-mode)

(use-package markdown-mode
  :config
  (push '("\\.markdown\\'" . markdown-mode) auto-mode-alist)
  (push '("\\.md\\'" . markdown-mode) auto-mode-alist))

(use-package mc-extras
  :demand
  :bind
  (:map mc/keymap
   ("C-c m =" . mc/compare-chars)))

(use-package ng2-mode)

(use-package org-projectile)

(use-package sass-mode)

(use-package smart-mode-line-powerline-theme
  :init
  (setq-default sml/theme 'powerline))

(use-package spinner)

(use-package sx
  :demand
  :bind
  (:map gpolonkai/pers-map
   ("qi" . sx-inbox)
   ("qs" . sx-search)))

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
  (("M-S-x" . helm-smex)))

(use-package ediff
  :init
  (setq-default ediff-merge-split-window-function 'split-window-horizontally
                ediff-split-window-function 'split-window-vertically
                ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package plantuml-mode
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
  :demand
  :config
  ;; Don’t bug me too often…
  (setq org-random-todo-how-often 3600)
  :bind
  (:map gpolonkai/pers-map
   ("r" . org-random-todo)))

(use-package calendar
  :ensure nil
  :init
  (setq calendar-week-start-day 1
        calendar-latitude 47.4
        calendar-longitude 19.0
        calendar-location-name "Budapest, Hungary"
        calendar-time-zone 60
        calendar-standard-time-zone-name "CET"
        calendar-daylight-time-zone-name "CEST"))

(use-package yasnippet
  :demand
  :config
  (yas-global-mode 1)
  :bind
  ;; Remove TAB binding to yas-expand.  It causes more harm than good.
  (:map yas-minor-mode-map
   ("TAB" . nil)
   ([(tab)] . nil)))

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

(use-package autorevert
  :config
  (global-auto-revert-mode 1))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook
            #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :config
  (add-hook 'css-mode-hook 'rainbow-mode)
  (add-hook 'scss-mode-hook 'rainbow-mode)
  (add-hook 'sass-mode 'rainbow-mode))

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
  (setq alert-default-style
        (if (termux-p)
            (progn
              ;; TODO Remove this as soon as my PR gets merged
              ;; https://github.com/jwiegley/alert/pull/41
              (unless (fboundp 'alert-termux-notify)
                (defcustom alert-termux-command (executable-find "termux-notification")
                  "Path to the termux-notification command.
This is found in the termux-api package, and it requires the Termux
API addon app to be installed."
                  :type 'file
                  :group 'alert)

                (defun alert-termux-notify (info)
                  "Send INFO using termux-notification.
Handles :TITLE and :MESSAGE keywords from the
INFO plist."
                  (if alert-termux-command
                      (let ((args (nconc
                                   (when (plist-get info :title)
                                     (list "-t" (alert-encode-string (plist-get info :title))))
                                   (list "-c" (alert-encode-string (plist-get info :message))))))
                        (apply #'call-process alert-termux-command nil
                               (list (get-buffer-create " *termux-notification output*") t)
                               nil args))
                    (alert-message-notify info)))

                (alert-define-style 'termux :title "Notify using termux"
                                    :notifier #'alert-termux-notify))
              'termux)
          'notifications)))

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

(use-package cheatsheet)

(use-package nxml-mode
  :ensure nil
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
  :after
  company
  :init
  (--set-emoji-font nil)
  :config
  (add-to-list 'company-backends 'company-emoji)
  (add-hook 'after-make-frame-functions
            '--set-emoji-font))

(use-package zygospore
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
  :config
  (ace-popup-menu-mode 1))

(use-package bind-key)

(use-package kanban)

(use-package achievements
  :config
  (achievements-mode 1))

(use-package auto-highlight-symbol
  :config
  (global-auto-highlight-symbol-mode t))

(use-package recentf
  :ensure nil
  :config
  (run-at-time nil (* 5 60) 'recentf-save-list)
  (add-to-list 'recentf-exclude (concat user-emacs-directory "elpa")))

(use-package auto-package-update
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

(use-package ace-mc)

(use-package dired-k
  :bind
  (:map dired-mode-map
   ("K" . dired-k)))

(use-package form-feed
  :config
  (add-hook 'emacs-lisp-mode-hook 'form-feed-mode))

(use-package anzu
  :config
  (global-anzu-mode 1))

(use-package auto-virtualenv
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  (add-hook 'projectile-after-switch-project-hook
            'auto-virtualenv-set-virtualenv))

(use-package flymake-python-pyflakes)

(use-package gitlab)

(use-package files
  :ensure nil
  :config
  (setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
  (setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t))))

(use-package helm-c-yasnippet
  :demand t
  :config
  (setq helm-yas-space-match-any-greedy t)
  :bind
  (("C-c y" . helm-yas-complete)))

(use-package helm-hunks)

(use-package helm-pydoc)

(use-package hl-todo)

(use-package glasses
  :config
  (add-hook 'prog-mode-hook 'glasses-mode))

(when (display-graphic-p)
  (use-package eshell-fringe-status
    :config
    (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode)))

(use-package eshell-prompt-extras
  :config
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda)))

(use-package expand-region
  :bind
  (:map ctl-x-map
   ("*" . er/expand-region)))

(use-package anaconda-mode
  :config
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package company-anaconda
  :after
  company
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package mediawiki
  :after
  id-manager
  :config
  (add-to-list 'mediawiki-site-alist
               '("WikEmacs"
                 "http://wikemacs.org/wiki/"
                 (gpolonkai/idm-get-id-for-account "WikEmacs")
                 (gpolonkai/idm-get-password-for-account "WikEmacs"))))

(use-package github-notifier
  :after
  id-manager
  :config
  (setq github-notifier-token (gpolonkai/idm-get-password-for-account "GitHub"))
  (github-notifier-mode))

(use-package gist)

(use-package company-web
  :config
  (require 'company-web-html))

(use-package enlive)

(use-package po-mode)

(use-package dashboard
  :after
  projectile
  :config
  (add-to-list 'dashboard-items '(projects . 5) t)
  (dashboard-setup-startup-hook))

(use-package csharp-mode)

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package dockerfile-mode)

(use-package phi-search)

(use-package phi-search-mc
  :config
  (phi-search-mc/setup-keys))

(use-package secretaria
  :after
  alert
  :config
  ;; use this for getting a reminder every 30 minutes of those tasks
  ;; scheduled for today and which have no time of day defined.
  (add-hook 'after-init-hook
            #'secretaria-today-unknown-time-appt-always-remind-me))

(when (termux-p)
  (use-package browse-url
    :ensure nil
    :config
    (advice-add 'browse-url-default-browser :override
                (lambda (url &rest args)
                  (start-process-shell-command
                   "open-url"
                   nil
                   (concat "am start -a android.intent.action.VIEW --user 0 -d "
                           url))))))

(use-package less-css-mode)

(use-package feature-mode)

(use-package helm-bibtex
  :after
  org
  :config
  (setq bibtex-completion-bibliography (concat user-documents-directory
                                               (convert-standard-filename
                                                "/orgmode/references.bib"))
        bibtex-completion-library-path (concat user-documents-directory
                                               (convert-standard-filename
                                                "/orgmode/bibtex-pdfs"))
        bibtex-completion-notes-path (concat user-documents-directory
                                             (convert-standard-filename
                                              "/orgmode/bibliography/helm-bibtex-notes"))
        bibtex-completion-pdf-open-function 'org-open-file))

(use-package org-ref
  :after
  org
  :config
  (setq org-ref-bibliography-notes (concat user-documents-directory
                                           (convert-standard-filename
                                            "/orgmode/bibliography-notes"))
        org-ref-default-bibliography '((concat user-documents-directory
                                               (convert-standard-filename
                                                "/orgmode/references.bib")))
        org-ref-pdf-directory (concat user-documents-directory
                                      (convert-standard-filename
                                       "/orgmode/bibtex-pdfs"))))

(use-package ag
  :after projectile
  :bind
  (:map projectile-mode-map
   ("C-c p C-a" . ag-project)))

;; open pdf with system pdf viewer (works on mac)
(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (start-process "open" "*open*" "open" fpath)))

;; alternative
;; (setq bibtex-completion-pdf-open-function 'org-open-file))

(add-hook 'python-mode-hook
          (lambda ()
            (add-to-list 'prettify-symbols-alist
                         '("not" . 172))
            (add-to-list 'prettify-symbols-alist
                         '("in" . 8712))
            (add-to-list 'prettify-symbols-alist
                         '("def" . 402))))

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
 ("C-s" . isearch-forward-regexp)
 ("C-r" . isearch-backward-regexp)
 ("C-M-s" . isearch-forward)
 ("C-M-r" . isearch-backward)
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
 ("C-b" . bury-buffer)
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

;; TODO: This doesn’t work with use-package and bind-key for some reason.
;; But why?
(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

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
(put 'set-goal-column 'disabled nil)

;; text-mode settings
(add-hook 'text-mode-hook (lambda () (visual-line-mode t)))

;; Add some symbols to be prettified
(setq prettify-symbols-alist
      '(("lambda" . 955)    ; λ
        ("function" . 402)  ; ƒ
        ("->" . 8594)       ; →
        ("=>" . 8658)       ; ⇒
        ("map" . 8614)      ; ↦
        ("not" . 172))      ; ¬

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

(defun gpolonkai/helm-ff-slash-dir-complete ()
  (interactive)
  (if (and (equal "Find Files" (assoc-default 'name (helm-get-current-source)))
           (stringp (helm-get-selection))
           (file-directory-p (helm-get-selection)))
      (helm-execute-persistent-action)
    (insert "/")))

(setq default-cursor-color (frame-parameter nil 'cursor-colbxuor))
(setq yasnippet-can-fire-cursor-color "purple")

;; It will test whether it can expand, if yes, cursor color -> purple.
(defun yasnippet-can-fire-p (&optional field)
  (interactive)
  (setq yas--condition-cache-timestamp (current-time))
  (let (templates-and-pos)
    (unless (and yas-expand-only-for-last-commands
                 (not (member last-command yas-expand-only-for-last-commands)))
      (setq templates-and-pos (if field
                                  (save-restriction
                                    (narrow-to-region (yas--field-start field)
                                                      (yas--field-end field))
                                    (yas--templates-for-key-at-point))
                                (yas--templates-for-key-at-point))))
    (and templates-and-pos (first templates-and-pos))))

;; Taken from http://pages.sachachua.com/.emacs.d/Sacha.html
(defun sachachua/change-cursor-color-when-can-expand (&optional field)
  (interactive)
  (when (eq last-command 'self-insert-command)
    (set-cursor-color (if (sachachua/can-expand)
                          yasnippet-can-fire-cursor-color
                        default-cursor-color))))

(defun sachachua/can-expand ()
  "Return true if right after an expandable thing."
  (or (abbrev--before-point) (yasnippet-can-fire-p)))

(add-hook 'post-command-hook 'sachachua/change-cursor-color-when-can-expand)

;; SysAdmin Day to the calendar
(add-to-list 'holiday-other-holidays '(holiday-float 7 5 -1 "SysAdmin Day") t)
