;;; init.el --- Init File -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

;;;; Setup package sources
(require 'package)
(setopt package-archives
	      '(("gnu"    . "https://elpa.gnu.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
	        ("melpa"  . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setopt package-native-compile t)

(use-package use-package
  :custom
  (use-package-expand-minimally t)
  (use-package-always-ensure t)
  (use-package-always-defer t)
  (use-package-enable-imenu-support t))


;;;; Defaults

(setopt inhibit-splash-screen t
	      inhibit-startup-screen t
	      inhibit-startup-message t
	      initial-scratch-message nil
	      kill-do-not-save-duplicates t
	      custom-safe-themes t
	      scroll-margin 2
	      select-enable-clipboard t
	      visible-bell nil
        ring-bell-function 'ignore
	      use-short-answers t
	      warning-minimum-level :error)

(put 'suspend-frame 'disabled t)

;;;; Multi OS support

(setq x-alt-keysym 'meta) ;; Alt as Meta key

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

;; (add-to-list 'default-frame-alist '(undecorated-round . t))

;; Ensure that environment variables are the same as the user’s shell
(when *is-a-mac*
  (use-package exec-path-from-shell
    :if (memq window-system '(mac ns x))
    :hook (exec-path-from-shell-initialize))
  (when (featurep 'ns)
    (defun ns-raise-emacs ()
      "Raise Emacs."
      (ns-do-applescript "tell application \"Emacs\" to activate"))

    (defun ns-raise-emacs-with-frame (frame)
      "Raise Emacs and select the provided frame."
      (with-selected-frame frame
	      (when (display-graphic-p)
	        (ns-raise-emacs))))
    (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame)
    (when (display-graphic-p)
      (ns-raise-emacs)))

  (setq ns-alternate-modifier 'meta
        ns-right-alternate-modifier 'none))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(when *is-a-linux*
  (use-package exec-path-from-shell
    :functions (exec-path-from-shell-initialize)
    :init (exec-path-from-shell-initialize)
    :custom (exec-path-from-shell-variables '("PATH" "SSH_AUTH_SOCK"))))

;;; Auth source
(use-package auth-source
  :ensure nil
  :custom
  (auth-sources'("~/.authinfo.gpg"))
  (auth-source-cache-exipry nil)
  (password-cache-expiry nil))

;;; Personal information
(setq user-full-name "Jonatan Borkowski"
      user-mail-address "jonatan@thebo.me")

;;; Prohibit littering
(use-package no-littering
  :demand
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (backup-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "backup/")))))

(setopt custom-file (make-temp-file "emacs-custom-"))

;;; Recent files
(use-package recentf
  :ensure nil
  :init (recentf-mode)
  :custom
  (recentf-max-menu-items 1000)
  (recentf-max-saved-items 1000))

;;; Autosave and backups
(use-package files
  :ensure nil
  :custom
  (auto-save-default t)
  (auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save" user-emacs-directory) t)))
  (make-backup-files t)
  (backup-directory-alist `((".*" . ,(expand-file-name "backup" user-emacs-directory))))
  (require-final-newline nil)
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-new-versions 6)
  (kept-old-versions 6)
  (create-lockfiles nil))


;;; Scratch buffer
(use-package scratch
  :ensure nil
  :bind ("C-c o s" . scratch-buffer))

;;; Project
(use-package project
  :ensure nil
  :bind (:map project-prefix-map ("g" . consult-ripgrep))
  :custom
  (project-switch-commands
   '((project-find-file "Find file" ?f)
     (project-dired "Find directory" ?d)
     (consult-ripgrep "Find regexp" ?g)
     (project-switch-to-buffer "Find buffer" ?b)
     (magit-project-status "Magit" ?m)
     (project-eshell "Eshell" ?e)))
  (project-vc-extra-root-markers '("hie.yaml" "package.json" "spago.dhall"))
  (project-vc-ignores '("node_modules" "output" "dist" "tmp")))

;;; History
(setq undo-limit 80000000
      history-length 5000
      history-delete-duplicates t)

;;;; Savehist

(use-package savehist
  :ensure nil
  :init (savehist-mode)
  :custom
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables
   '(kill-ring search-ring regexp-search-ring)))

(use-package saveplace
  :ensure nil
  :init (save-place-mode)
  :custom (save-place-forget-unreadable-file t))

;;; Dired
(use-package dired
  :ensure nil
  :hook (dired-mode . hl-line-mode)
  :custom
  (delete-by-moving-to-trash t)
  ;;  (dired-listing-switches "-alGh --group-directories-first")
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-dwim-target t))

;;; Helpers
(use-package help
  :ensure nil
  :custom
  (help-window-select t)
  (help-link-key-to-documentation t))

(use-package eldoc
  :ensure nil
  :init (global-eldoc-mode)
  :custom
  (eldoc-idle-delay 0.1)
  (eldoc-mode-line-string nil)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-display-truncation-message nil)
  (eldoc-documentation-function 'eldoc-documentation-compose))

;;; Xref
(use-package xref
  :ensure nil
  :custom
  (xref-auto-jump-to-first-definition 'show)
  (xref-search-program 'ripgrep))

;;; Appearance

;;; Vertical separator

(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(setopt mode-line-position-line-format `(" %l:%c")
	      mode-line-position-column-line-format '(" %l,%c")
	      mode-line-end-spaces nil
	      mode-line-compact nil)

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                mode-line-mule-info
                mode-line-client
                mode-line-modified
                mode-line-remote
                mode-line-frame-identification
                mode-line-buffer-identification
                "  "
                mode-line-position
                "  "
                (vc-mode vc-mode)
                "  "
                mode-line-modes
                "  "
                mode-line-misc-info
                mode-line-end-spaces))

(use-package minions
  :functions (minions-mode)
  :init (minions-mode)
  :custom (minions-mode-line-lightr "..."))

;;;; Theme
(setopt custom-safe-themes t)

(use-package modus-themes
  :pin melpa
  :bind
  ("C-c t t" . modus-themes-toggle)
  :custom
  (modus-themes-to-toggle
   '(modus-operandi-tinted modus-vivendi-tinted))
  (modus-themes-common-palette-overrides
	 '((prose-done green-intense)
	   (prose-todo red-intense)))
  
  ;; Tone down almost all colors.
  (modus-themes-common-palette-overrides)
	(modus-themes-preset-overrides-faint))


(defun bore/os-theme ()
  "Get os theme."
  (downcase
   (if *is-a-linux*
       (shell-command-to-string "gsettings get org.gnome.desktop.interface color-scheme")
     (shell-command-to-string "defaults read -g AppleInterfaceStyle"))))

(if (string-match-p "dark" (bore/os-theme))
    (load-theme 'modus-vivendi-tinted)
  (load-theme 'modus-operandi-tinted))

(use-package nyan-mode
  :init (nyan-mode))

;;;; Fonts
(defun bore/with-font-faces-mac ()
  "Setup all Emacs font faces."
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font (font-spec :family "Iosevka" :size 14 :weight 'regular))
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family "Iosevka" :size 14 :weight 'regular))
    (set-face-attribute 'variable-pitch nil :font (font-spec :family "Iosevka Aile" :size 14 :weight 'regular))))

(defun bore/with-font-faces-linux ()
  "Setup all Emacs font faces."
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font (font-spec :family "CommitMono" :size 14 :weight 'regular))
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family "CommitMono" :size 14 :weight 'regular))
    (set-face-attribute 'variable-pitch nil :font (font-spec :family "Inter" :size 15 :weight 'regular))))

(when *is-a-mac* 
  (add-hook 'after-init-hook 'bore/with-font-faces-mac)
  (add-hook 'server-after-make-frame-hook 'bore/with-font-faces-mac))


(when *is-a-linux*
  (add-hook 'after-init-hook 'bore/with-font-faces-linux)
  (add-hook 'server-after-make-frame-hook 'bore/with-font-faces-linux))

;;;; Encoding

(setq-default default-buffer-file-coding-system 'utf-8
              buffer-file-coding-system 'utf-8)

;; Make those lambdas pretty again
(global-prettify-symbols-mode t)

;; For the first time remember to run M-x all-the-icons-install-fonts
(use-package all-the-icons)

(setopt display-line-numbers-type nil)

;; But for sure disable line numbers in some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                eshell-mode-hool))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


(use-package subword
  :ensure nil
  :init (global-subword-mode))

(use-package delsel
  :ensure nil
  :init (delete-selection-mode))

(use-package whitespaces
  :ensure nil
  :bind
  ("C-c w t" . whitespace-mode)
  ("C-c w c" . whitespace-cleanup))

;;;; Tabs
(setq-default tab-width 2
	            tab-always-indent 'complete
	            indent-tabs-mode nil)

(use-package tab-bar
  :ensure nil
  :custom
  (setq tab-bar-close-button-show nil
        tab-bar-new-button nil
        tab-bar-separator " "
        tab-bar-show 1)
  :init
  (setq tab-bar-new-tab-to 'rightmost
        tab-bar-close-tab-select 'recent
        ;; set default tab name to current buffer.
        ;; alternative is to set new tab to scratch - tab-bar-new-tab-choice "*scratch*"
        tab-bar-new-tab-choice t
        tab-bar-tab-name-function 'tab-bar-tab-name-current
        tab-bar-format '(tab-bar-format-history tab-bar-format-tabs)
        ))

(tab-bar-history-mode 1)

;;;; Formatting

(setq-default fill-column 80
	            wrap-word t
	            tuncate-lines t)

(setq scroll-conservatively 101                    ; value greater than 100 gets rid of half page jumping
      mouse-wheel-scroll-amount '(3 ((shift) . 3)) ; how many lines at a time
      mouse-wheel-progressive-speed t              ; accelerate scrolling
      mouse-wheel-follow-mouse 't)                 ; scroll window under mouse


;;;; Parentheses
(use-package paren
  :ensure nil
  :init (show-paren-mode)
  :custom
  (show-paren-delay 0)
  (show-paren-highlight-openparen t)
  (show-paren-when-point-inside-paren t))

;;;; Electric Behaviour
(use-package elec-pair
  :ensure nil
  :init (electric-pair-mode)
  :custom
  (electric-pair-inhibit-predicate'electric-pair-conservative-inhibit)
  (electric-pair-skip-self 'electric-pair-default-skip-self)
  (electric-pair-skip-whitespace nil)
  (electric-pair-preserve-balance t)
  (electric-indent-mode 1))


;;;; Rainbow
(use-package rainbow-mode
  :bind ("C-c t r" . rainbow-mode))

;;; Search

;;;; Isearch
(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map ("M-/" . isearch-complete))
  :custom
  (isearch-lazy-count t)
  (isearch-whitespace-regexp ".*?")
  (isearch-allow-scroll 'unlimited))

;;;; Occur
(use-package occur
  :ensure nil
  :hook (occur-mode . hl-line-mode))

;;;; Wgrep
(use-package wgrep
  :defines (grep-mode-map wgrep-mode-map)
  :bind
  (:map grep-mode-map
        ("e"       . wgrep-change-to-wgrep-mode)
        ("C-x C-q" . wgrep-change-to-wgrep-mode))
  (:map wgrep-mode-map ("C-c C-c" . wgrep-finish-edit))
  :custom (wgrep-auto-save-buffer t))

;;;; Avy
(use-package avy
  :bind (("C-'" . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-'" . avy-isearch))
  :custom
  (avy-keys '(?n ?e ?i ?s ?t ?r ?i ?a))
  (avy-timeout-seconds .3)
  (avy-background t))


;;; Buffer, frames and windows

;;;; Buffers
(use-package ibuffer
  :ensure nil
  :bind (("C-x C-b" . ibuffer))
  :custom
  (ibuffer-expert t)
  (ibuffer-display-summary nil)
  (ibuffer-use-other-window nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-movement-cycle nil)
  (ibuffer-default-sorting-mode 'filename/process)
  (ibuffer-use-header-line t)
  (ibuffer-default-shrink-to-minimum-size nil)
  (ibuffer-saved-filter-groups nil)
  (ibuffer-old-time 48)
  (add-hook 'ibuffer-mode-hook #'hl-line-mode))

(use-package autorevert
  :ensure nil
  :init
  (global-auto-revert-mode)
  :custom
  (global-auto-revert-non-file-buffers t))

;;;; Frames
(setopt frame-resize-pixelwise t
	      focus-follows-mouse t)

(add-hook 'after-init-hook #'pixel-scroll-precision-mode)

;;;; Windows

(use-package window
  :ensure nil
  :bind
  ("C-x O"   . other-other-window)
  ("C-x C-n" . next-buffer)
  ("C-x C-p" . previous-buffer)
  ("C-x k"   . kill-current-buffer)
  ("C-x K"   . kill-buffer)
  ("C-c q q" . kill-emacs)
  ("C-c q r" . restart-emacs)
  (:repeat-map other-window-repeat-map
	             ("o" . other-window)
	             ("O" . other-other-window))
  :custom
  (window-resize-pixelwise t)
  (window-combination-resize t)
  (recenter-positions '(top middle bottom))
  (scroll-preserve-screen-position t)
  (switch-to-buffer-in-dedicated-window 'pop)
  :config
  (defun other-other-window ()
    "Go to previous window."
    (interactive)
    (other-window -1)))

(use-package windmove
  :ensure nil
  :init (windmove-default-keybindings)
  :custom
  (windmove-default-keybindings '(nil . (shift)))
  (windmove-swap-states-default-keybindings '(nil . (control shift))))

;;;; Winner
(use-package winner
  :ensure nil
  :hook
  (after-init . winner-mode))


;;; Completion

;;;; Minibuffer
(use-package minibuffer
  :ensure nil
  :defines (crm-separator)
  :functions (crm-indicator)
  :hook (minibuffer-setup . cursor-intangible-mode)
  :custom
  (completion-ignore-case t)
  (completion-auto-select t)
  (completion-auto-help 'visible)
  (completion-show-help nil)
  (completions-detailed t)
  (completions-header-format nil)
  (completions-format 'one-column)

  ;; Tweak minibuffer behaviour
  (resize-mini-windows t)
  (enable-recursive-minibuffers t)
  ;; (minibuffer-depth-indicate-mode t)
  (minibuffer-electric-default-mode t)
  (minibuffer-eldef-shorten-default t)

  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate
   #'command-completion-default-include-p)
  :config
  (defun crm-indicator (args)
    "Add prompt indicator to `completing-read-multiple'.
 We display [CRM<separator>], e.g., [CRM,] if the separator is a comma."
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator))

;;;; Vertico
(use-package vertico
  :defines (vertico-map)
  :functions (vertico-mode)
  :init (vertico-mode)
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind
  (:map vertico-map
        ("M-j" . vertico-quick-exit))
  :custom
  (vertico-mouse-mode t)
  (vertico-scroll-margin 0)
  (vertico-resize t)
  (vertico-cycle t)
  (vertico-count 15))


;;;; Orderless
(use-package orderless
  :custom
  (completion-styles '(orderless flex))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

;;;; Marginalia
(use-package marginalia
  :functions (marginalia-mode)
  :init (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil)))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind
  ("C-x C-r" . consult-recent-file)
  ("C-x b"   . consult-buffer)
  ("C-x r b" . consult-bookmark)
  ("C-x 4 b" . consult-buffer-other-window)
  ("C-x 5 b" . consult-buffer-other-frame)
  ("C-c c x" . consult-flymake)

  ("M-y"     . consult-yank-pop)
  ("M-g e"   . consult-compile-error)
  ("M-g f"   . consult-flycheck)
  ("M-g g"   . consult-goto-line)
  ("M-g M-g" . consult-goto-line)
  ("M-g o"   . consult-outline)
  ("M-g m"   . consult-mark)
  ("M-g k"   . consult-global-mark)
  ("M-g i"   . consult-imenu)

  ("M-s f" . consult-find)
  ("M-s F" . consult-locate)
  ("M-s g" . consult-grep)
  ("M-s G" . consult-git-grep)
  ("M-s r" . consult-ripgrep)
  ("M-s l" . consult-line)
  ("M-s L" . consult-line-multi)
  ("M-s m" . consult-multi-occur)
  ("M-s k" . consult-keep-lines)
  ("M-s u" . consult-focus-lines)

  ("M-s e" . consult-isearch-history)
  (:map isearch-mode-map
	      ("M-e" . consult-isearch-history)          ; o
	      ("M-s e" . consult-isearch-history)
	      ("M-s l" . consult-line))

  :custom
  (xref-show-xrefs-function       #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (completion-in-region-function  #'consult-completion-in-region)
  (consult-preview-key "M-.")
  (consult-narrow-key "<"))

;;;; Consult Dir
(use-package consult-dir
  :custom (consult-dir-shadow-filenames nil)
  :bind
  ("C-x C-d" . consult-dir)
  (:map vertico-map
	      ("C-x C-d" . consult-dir)
	      ("C-x C-j" . consult-dir-jump-file)))


;;;; Embark

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-," . embark-dwim)
   ("C-h B" . embark-bindings))

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	             '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		             nil
		             (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))


;;;; Cape

(use-package cape
  :functions
  (cape-file cape-dabbrev cape-keyword cape-wrap-silent cape-wrap-purify)
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))


(use-package strokes-mode
  :ensure nil
  :bind ("S-<down-mouse-2>" . strokes-do-stroke)
  :commands (strokes-do-stroke strokes-global-set-stroke)
  :custom
  (strokes-mode t))

;;;; Pcmpl-args - Shell completions

(use-package pcmpl-args
  :after (vertico))

;;;; Tempel

(use-package tempel
  :bind (("M-+" . tempel-complete)
	       ("M-*" . tempel-insert)))


;;;; Hippie-expand

(use-package hippie-exp
  :ensure nil
  :bind ("M-/" . hippie-expand)
  :custom
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev-visible
     try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     try-expand-list
     try-expand-line)))

;;; Editing

;;;; Olivetti

(use-package olivetti
  :bind ("C-c t o" . olivetti-mode)
  :custom
  (olivetti-body-width 180)
  (olivetti-minimum-body-width 100)
  (olivetti-recall-visual-line-mode-state t))

;;;; Logos

(use-package logos
  :after (outline)
  :bind
  ([remap narrow-to-region] . logos-narrow-dwim)
  ([remap forward-page]     . logos-forward-page-dwim)
  ([remap backward-page]    . logos-backward-page-dwim)
  ("C-c F"                  . logos-focus-mode)
  :custom
  (logos-outlines-are-pages t)
  (logos-hide-mode-line t)
  (logos-scroll-lock nil)
  (logos-variable-pitch nil)
  (logos-indicate-buffer-boundaries nil)
  (logos-buffer-read-only nil)
  (logos-olivetti t))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind ("M-$" . jinx-correct)
  :custom
  (jinx-languages "en_US pl_PL es_ES de")
  (jinx-exclude-regexps
   '((t "[A-Z]+\\>"
        "\\<[[:upper:]][[:lower:]]+\\>"
        "\\w*?[0-9\.'\"-]\\w*"
        "[a-z]+://\\S-+"
        "<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?"))))

;;;; Flyspell
(use-package flyspell
  :hook ((message-mode git-commit-mode org-mode text-mode) . flyspell-mode)
  :bind
  ("C-c t s" . flyspell-mode)
  (:map flyspell-mode-map
        ("C-." . nil)
        ("C-;" . nil))
  :config
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))



;;;; Clipetty

(use-package clipetty
  :unless (display-graphic-p)
  :hook (tty-setup . global-clipetty-mode))



;;;; Org

(use-package org
  :hook (org-mode . turn-on-visual-line-mode)
  :commands org-capture org-agenda
  :init
  (add-hook 'org-mode-hook
	          (lambda ()
	            (variable-pitch-mode 1)
	            (org-modern-mode)
	            (visual-line-mode 1)))
  :custom
  (org-directory "~/org/")
  (org-adapt-indentation nil)
  (org-edit-src-persistent-message nil)
  (org-fold-catch-invisible-edits 'show-and-error)
  (org-insert-heading-respect-content t)
  (org-fontify-quote-and-verse-blocks t)
  (org-tags-column 0)
  (org-hide-emphasis-markers t)
  (org-hide-macro-markers t)
  (org-hide-leading-stars nil)
  (org-ellipsis "…")
  (org-capture-bookmark nil)
  (org-mouse-1-follows-link t)
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil)
  (org-indirect-buffer-display 'current-window)
  (org-eldoc-breadcrumb-separator " → ")
  (org-enforce-todo-dependencies t)
  (org-startup-folded t)
  (org-use-sub-superscripts '{})
  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-fontify-whole-heading-line t)
  (org-capture-bookmark nil)
  (org-priority-faces
   '((?A . error)
     (?B . warning)
     (?C . success)))

  (org-entities-user
   '(("flat"  "\\flat" nil "" "" "266D" "♭")
     ("sharp" "\\sharp" nil "" "" "266F" "♯")))

  (org-imenu-depth 6)
  (org-structure-template-alist
   '(("s" . "src")
     ("e" . "src emacs-lisp")
     ("h" . "src haskell")
     ("E" . "example")
     ("q" . "quote")
     ("c" . "comment")))
  (org-capture-templates
   '(("t" "Todo" entry (file+headline "~/org/inbox.org" "True Life Tasks")
      "* TODO %? \n%U" :empty-lines 1)
     ("w" "Todo (work)" entry (file+headline "~/org/inbox.org" "Work Tasks")
      "* TODO %? \n%U" :empty-lines 1)
     ("e" "Event" entry (file+headline "~/org/agenda.org" "Agenda")
      "** %? \n %^T\n%U" :empty-lines 1))))


(use-package org-modern
  :after (org)
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-mode)
  :custom
  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.2)
  (dolist (face '((org-level-1 . 1.15)
		              (org-level-2 . 1.10)
		              (org-level-3 . 1.05)
		              (org-level-4 . 1.0)
		              (org-level-5 . 1.1)
		              (org-level-6 . 1.1)
		              (org-level-7 . 1.1)
		              (org-level-8 . 1.1)))))


(use-package org-appear
  :hook (org-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-autoemphasis t)
  (org-appear-autoentities t)
  (org-appear-autokeywords t))


(use-package org-agenda
  :ensure nil
  :bind
  ("C-c a" . org-agenda)
  ("C-c x" . org-capture)
  :custom
  (org-agenda-files (list org-directory))
  (org-agenda-compact-blocks nil)
  (org-agenda-window-setup 'current-window)
  (org-agenda-skip-unavailable-files t)
  (org-agenda-span 10)
  (calendar-week-start-day 1)
  (org-agenda-start-on-weekday nil)
  (org-agenda-start-day "-3d")
  (org-agenda-deadline-faces
   '((1.001 . error)
     (1.0 . org-warning)
     (0.5 . org-upcoming-deadline)
     (0.0 . org-upcoming-distant-deadline)))
  (org-agenda-inhibit-startup t))

(use-package org-cliplink
  :bind ("C-c l" . 'org-cliplink))

;;;; Detote

(use-package denote
  :bind
  ("C-c n n" . denote)
  ("C-c n N" . denote-type)
  ("C-c n i" . denote-link)
  ("C-c n b" . denote-link-backlinks)
  ("C-c n l" . denote-link-find-file)
  ("C-c n r" . denote-rename-file)
  ("C-c n j" . bore/journal)
  ("C-c n f" . consult-notes)
  :functions (denote--title-prompt)
  :defines (denote-directory)
  :hook (dired-mode . denote-dired-mode-in-directories)
  :custom
  (denote-directory (expand-file-name "~/org/notes/"))
  (denote-known-keywords '("linux" "journal" "emacs" "embedded" "hobby"))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-prompt-for-date-return-id '(title keywords))
  (denote-front-matter-date-format 'org-timestamp)
  (denote-templates '((todo . "* Tasks:\n\n")))
  (denote-file-type 'markdown-yaml) ; Org is the default, set others here
  :config
  (defun bore/journal ()
    "Create an entry tagged 'journal' with the date as its title"
    (interactive)
    (denote
     (format-time-string "%A %e %B %Y")
     '("journal"))))

(use-package consult-notes
  ;;  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
	           consult-notes-search-in-all-notes
	           consult-notes-org-roam-find-node
	           consult-notes-org-roam-find-node-relation)
  :custom
  (consult-notes-file-dir-sources
   `(("Notes" ?n "~/org/notes")
     ("Roam"  ?r "~/org/roam")))
  (when (locate-library "denote")
    (consult-notes-denote-mode)))

;; use https://tony-zorman.com/posts/vc-use-package.html
;; (when *is-a-mac*
;;   (use-package orgmark
;;     :straight (orgmark
;; 	       :host github
;; 	       :repo "casouri/OrgMark")))

;;;; Markdown

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook (markdown-mode . turn-on-visual-line-mode)
  :custom
  (markdown-fontify-code-blocks-natively t))

;;;; Enable focus on block with C-c '
(use-package edit-indirect)

;;;; Citar
(use-package citar
  :hook (org-mode  . citar-capf-setup)
  :bind ("C-c n c" . citar-insert-citation)
  :custom
  (org-cite-global-bibliography '("~/org/bibliography.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography))

(use-package citar-embark
  :after (citar embark)
  :functions (citar-embark-mode)
  :init (citar-embark-mode)
  :custom (citar-at-point-function 'embark-act))

(use-package citar-denote
  :after (citar denote)
  :functions (citar-denote-mode)
  :init (citar-denote-mode)
  :custom (citar-open-always-create-notes t))


;;; Utility

(use-package which-key
  :defer t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))

(use-package proced
  :ensure nil
  :custom (proced-auto-update-flag t))

(use-package xclip
  :unless (display-graphic-p)
  :hook (tty-setup)
  :custom
  (when *is-a-linux*
    xclip-method 'wl-copy)
  (when *is-a-mac*
    xclip-method 'pbcopy))

(add-hook 'tty-setup-hook #'xterm-mouse-mode)

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(setopt vc-handled-backends '(Git)
        vc-follow-symlinks t)

(use-package hl-todo
  :hook (prog-mode))

(use-package evil-multiedit
  :defer t)

(use-package multiple-cursors
  :bind
  ("C-<" . mc/mark-previous-like-this)
  ("C->" . mc/mark-next-like-this)
  ("C-c C-<" . mc/mark-all-like-this)
  ("C-S-<mouse-1>" . mc/add-cursor-on-click))

;;; Web browsing

(use-package shr
  :ensure nil
  :custom
  (shr-use-colors nil)
  (shr-use-fonts nil)
  (shr-max-image-proportion 0.6)
  (shr-image-animate nil)
  (shr-width nil)
  (shr-discard-aria-hidden t)
  (shr-cookie-policy nil))

(use-package eww
  :ensure nil
  :bind ("C-c o b" . eww)
  :custom
  
  (eww-search-prefix "https://html.duckduckgo.com/html/?q=")
  (eww-download-directory (expand-file-name "~/Downloads"))
  (eww-history-limit 150)
  (eww-browse-url-new-window-is-tab nil)
  (eww-form-checkbox-selected-symbol "[X]")
  (eww-form-checkbox-symbol "[ ]")
  (eww-retrieve-command nil))

;;; RSS

(use-package elfeed
  :bind ("C-x w" . elfeed))

(use-package elfeed-org
  :after (elfeed)
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files (list "~/org/elfeed.org")))

;;; Email (linux only)

(when *is-a-linux*
  (use-package message
    :ensure nil
    :custom
    (message-directory "~/.mail/thebo")
    (message-kill-buffer-on-exit t)
    (message-wide-reply-confirm-recipients t)
    (message-sendmail-envelope-from 'header))

  (use-package notmuch
    :defines (notmuch-show-mode-map)
    :hook
    (notmuch-message-mode . turn-off-auto-fill)
    (notmuch-message-mode . turn-on-visual-line-mode)
    :bind
    ("C-c o m" . notmuch)
    (:map notmuch-show-mode-map ("o" . notmuch-show-interactively-view-part))
    :custom
    (notmuch-show-logo nil)
    (notmuch-column-control t)
    (notmuch-hello-auto-refresh t)
    (notmuch-show-all-tags-list t)
    (notmuch-show-empty-saved-searches t)
    (notmuch-hello-recent-searches-max 20)

    ;; Search functionality
    (notmuch-search-oldest-first nil)
    (notmuch-search-result-format
     '(("date" . "%12s ")
       ("count" . "%-7s ")
       ("authors" . "%-30s ")
       ("subject" . "%-72s ")
       ("tags" . "(%s)")))
    (notmuch-saved-searches
     '((:name "inbox"    :query "tag:inbox not tag:trash" :key "i")
       (:name "flagged"  :query "tag:flagged"             :key "f")
       (:name "sent"     :query "tag:sent"                :key "s")
       (:name "drafts"   :query "tag:drafts"              :key "d")
       (:name "archived" :query "tag:archived"            :key "a")))

    ;; Tags functionality
    (notmuch-archive-tags '("-inbox" "-unread" "+archived"))
    (notmuch-message-replied-tags '("+replied"))
    (notmuch-message-forwarded-tags '("+forwarded"))
    (notmuch-show-mark-read-tags '("-unread"))
    (notmuch-draft-tags '("+drafts"))
    (notmuch-tagging-keys
     '(("u" ("+unread") "Mark as unread")
       ("f" ("+flag" "-unread") "Flag as important")
       ("r" notmuch-show-mark-read-tags "Mark as read")
       ("s" ("+spam" "-inbox" "-unread") "Mark as spam")
       ("a" notmuch-archive-tags "Archive (remove from inbox)")
       ("d" ("+trash" "-inbox" "-archived" "-unread"
	           "-git" "-services" "-stores" "-networks" "-billing")
	      "Mark for deletion")))

    ;; Reading messages
    (notmuch-show-relative-dates t)
    (notmuch-message-headers-visible t)

    ;; Email composition
    (notmuch-always-prompt-for-sender t)
    (notmuch-mua-compose-in 'current-window)
    (notmuch-mua-user-agent-function 'notmuch-mua-user-agent-full)
    (notmuch-mua-cite-function 'message-cite-original-without-signature)

    ;; Directory for sent messages
    (notmuch-fcc-dirs "thebo/Sent"))

  (use-package sendmail
    :ensure nil
    :custom
    (mail-specify-envelope-from t)
    (mail-user-agent 'message-user-agent)
    (smtpmail-smtp-server "smtp.migadu.com")
    (smtpmail-smtp-service 465)
    (smtpmail-stream-type 'ssl)
    (send-mail-function 'smtpmail-send-it)))

(use-package org-mime
  :after (org notmuch)
  :custom (org-mime-library 'mml org-mime-export-ascii 'utf-8))

;;; Shell
(use-package shell
  :ensure nil
  :custom (shell-file-name "zsh"))

(use-package eat
  :commands (eat)
  :hook
  (eshell-mode . eat-eshell-mode)
  (eshell-mode . eat-eshell-visual-command-mode)
  :bind
  ("C-c o T" . eat-shell-other-window)
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-enable-shell-prompt-annotation nil)
  :config
  
  (defun eat-shell-other-window (&optional arg)
    "Open a `eat' in a new window."
    (interactive "P")
    (let ((buf (eat arg)))
      (switch-to-buffer (other-buffer buf))
      (switch-to-buffer-other-window buf))))

;;; Eshell
(use-package eshell
  :ensure nil
  :bind
  ("C-c o t" . eshell-other-window)
  :custom
  (eshell-history-size 10000)
  (eshell-hist-ignoredups t)
  (eshell-banner-message "")
  (eshell-kill-processes-on-exit t)
  (eshell-scroll-to-bottom-on-input t)
  (eshell-highlight-prompt t)
  (eshell-where-to-jump 'begin)
  (eshell-review-quick-commands nil)
  (eshell-smart-space-goes-to-end t)
  (eshell-destroy-buffer-when-process-dies t)
  :config
  (require 'em-hist)
  (require 'em-smart)
  (setenv "PAGER" "cat")

  (defun eshell-other-window ()
    "Open a `eshell' in a new window."
    (interactive)
    (let ((buf (eshell)))
      (switch-to-buffer (other-buffer buf))
      (switch-to-buffer-other-window buf))))

;;;; Tramp

(use-package tramp
  :ensure nil
  :custom
  (tramp-default-method "ssh")
  (tramp-terminal-type "tramp")
  (tramp-verbose 6))

;;; Magit

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-diff-refine-hunk t)
  (magit-save-repository-buffers 'dontask)
  (magit-revision-insert-related-refs nil)
  (magit-section-visibility-indicator nil)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


;;;; Ediff
(use-package ediff
  :ensure nil
  :custom
  (ediff-merge-split-window-function 'split-window-horizontally)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

;;;; Diff-hl

(use-package diff-hl
  :functions (global-diff-hl-mode diff-hl-margin-mode)
  :hook (dired-mode . diff-hl-dired-mode)
  :custom
  (diff-hl-draw-borders nil)
  (diff-hl-margin-symbols-alist
   '((insert  . " ")
     (delete  . " ")
     (change  . " ")
     (unknown . " ")
     (ignored . " ")))
  :config
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  (global-diff-hl-mode 1))


(use-package diff-hl
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode 1))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode))

(use-package envrc
  :config
  (envrc-global-mode))

(use-package eglot
  :ensure nil
  :hook ((js-ts-mode
          haskell-mode
          purescript-mode
          typescript-ts-mode
          rust-ts-mode
          c-ts-mode
          c++-ts-mode) . eglot-ensure)

  :bind
  (:map prog-mode-map
        ("C-c c l" . eglot)
        ("C-c c q" . eglot-shutdown))
  (:map eglot-mode-map
        ("C-c c x" . consult-flymake)
        ("C-c c a" . eglot-code-actions)
        ("C-c c r" . eglot-rename)
        ("C-c c f" . eglot-format)
        ("C-c c d" . eldoc))
  :custom
  (read-process-output-max (* 1024 1024))
  (eglot-events-buffer-size 0)
  (eglot-sync-connect 1)
  (eglot-autoshutdown t)
  (eglot-extend-to-xref t)
  (eglot-confirm-server-initiated-edits nil)
  
  ;; (eglot-ignored-server-capabilities
  ;;  '(:codeLensProvider
  ;;    :documentHighlightProvider
  ;;    :documentFormattingProvider 
  ;;    :documentRangeFormattingProvider))
  
  :config
  (add-to-list 'eglot-server-programs
               '(yaml-ts-mode . ("dsl" "lsp"))
               '((c++-mode c-mode) "clangd"))

  (setq-default eglot-workspace-configuration
                '((:purescript . (:addSpagoSources t :addNpmPath t)))))


;;;; Tree sitter
(use-package treesit-auto
  :demand
  :functions (global-treesit-auto-mode)
  :custom (treesit-auto-install t)
  :config
  (add-to-list 'auto-mode-alist '("[/\\]\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/\\]*\\)?\\'" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("CMakeLists\\'" . cmake-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'"      . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'"     . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.rs\\'"      . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.lalrpop\\'" . rust-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'"   . yaml-ts-mode))
  (global-treesit-auto-mode))

;;;; Compilation
(use-package compile
  :functions (ansi-color-apply-on-region)
  :hook
  (haskell-mode       . haskell-compiler)
  (purescript-mode    . purescript-compiler)
  (compilation-filter . colorize-compilation-buffer)
  (rust-ts-mode       . rust-compiler)
  :bind
  (:map prog-mode-map
        ("C-c c c" . compile)
        ("C-c c C" . recompile))
  :custom (compilation-scroll-output t)
  :config
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    "Colorize compilation buffer."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (defun haskell-compiler ()
    (setq-local compile-command "stack build --fast --copy-bins"))

  (defun purescript-compiler ()
    (setq-local compile-command "npm run pbuild"))
  
  (defun rust-compiler ()
    (setq-local compile-command "cargo build")))

;;;; Syntax checker

(use-package flymake
  :ensure nil
  :hook (prog-mode)
  :bind
  ("C-c t f" . flymake-mode)
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error))
  :custom
  (flymake-suppress-zero-counters t)
  (flymake-mode-line-counter-format
   '(" " flymake-mode-line-error-counter
     flymake-mode-line-warning-counter
     flymake-mode-line-note-counter "")))

(use-package flymake-collection
  :functions (flymake-collection-hook-setup)
  :init (flymake-collection-hook-setup)
  :custom (flymake-collection-hook-ignore-modes
           '(eglot--managed-mode bash-ts-mode)))

;;;; Apheleia
(use-package apheleia
  :defines (formatter-cmd formatter-mode)
  :functions (apheleia-global-mode apheleia-formatters apheleia-mode-alist)
  :bind ("C-c t a" . apheleia-mode)
  :init (apheleia-global-mode)
  :config
  (dolist (formatter-cmd '((fourmolu  . ("fourmolu" "--indentation" "2" "--stdin-input-file"
                                         (or (buffer-file-name) (buffer-name))))))
    (add-to-list #'apheleia-formatters formatter-cmd))

  (dolist (formatter-mode '((emacs-lisp-mode . lisp-indent)
                            (haskell-mode    . fourmolu)))
    (add-to-list #'apheleia-mode-alist formatter-mode)))

;;;; Direnv

(use-package envrc
  :functions (envrc-global-mode)
  :init (envrc-global-mode))

;;; Programming Langs

;;;; Lua

(use-package lua-mode
  :mode "\\.lua\\'"
  :custom
  (lua-indent-level 2))

;;;; Yaml

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;;;; Ocaml

(use-package tuareg
  :mode ("\\.ml[iylp]?$" . tuareg-mode))

(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

;;;; Terrafrom

(use-package terraform-mode)

;;;; Rust

(use-package rust-mode
  :mode
  ("\\.rs\\'"      . rust-mode)
  ("\\.lalrpop\\'" . rust-mode)
  :defines (rust-mode-map)
  :bind
  (:map rust-mode-map
        ("M-j" . lsp-ui-imenu)
        ("M-?" . lsp-find-references)
        ("C-c c l" . flycheck-list-errors)
        ("C-c c x" . lsp-execute-code-action)
        ("C-c c b" . rustic-cargo-build)
        ("C-c c r" . lsp-rename)

        ("C-c c s" . lsp-rust-analyzer-status)
        ("C-c c A" . rustic-cargo-add-missing-dependencies))
  
  )

;;;; JS

(use-package js-mode
  :ensure nil
  :custom
  (js--prettify-symbols-alist nil)
  (js-indent-level 2)
  (typescript-indent-level 2)
  (js-switch-indent-offset 2))

;;;; HTML

(use-package web-mode
  :defines (web-mode-map)
  :mode
  ("\\.html?\\'" . web-mode)
  ("\\.hbs\\'"   . web-mode)
  :bind
  (:map web-mode-map ("C-c C-a" . nil)) ; reserved for `embark-act'
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing nil)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t))

;;;; CSS

(use-package css-mode
  :ensure nil
  :custom (css-indent-offset 2))

;;;; PureScript

(use-package purescript-mode
  :mode ("\\.purs\\'")
  :hook (purescript-mode . turn-on-purescript-indentation))

;;;; Haskell

(use-package haskell-mode
  :mode
  ("\\.hs\\'"    . haskell-mode)
  ("\\.cabal\\'" . haskell-cabal-mode)
  :hook
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . haskell-indentation-mode)
  :custom
  (haskell-interactive-popup-errors nil)
  (haskell-process-log t)
  (haskell-process-load-or-reload-prompt t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-suggest-hoogle-imports t)
  (haskell-process-suggest-remove-import-lines t))

;;;; TS
(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . prettier-format-on-save-mode)
  :custom
  (typescript-indent-level 2))

;;;; PS
(use-package purescript-mode
  :mode ("\\.purs\\'")
  :hook (purescript-mode . turn-on-purescript-indentation))

;;;; Scheme/Lisp
(use-package slime
  :defines inferior-lisp-program
  :config
  (setq inferior-lisp-program "sbcl"))


;;;; Hare (only for linux)
(when *is-a-linux* 
  (use-package hare-mode
    :vc (:fetcher sourcehut :repo laumann/hare-mode)))

(use-package zig-mode
  :mode ("\\.zig\\'"))

(load "mwheel")
(mouse-wheel-mode 1)
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount
      '(1 ((shift) . 1)
          ((control) . nil)))

;; Local Variables:
;; byte-compile-warnings: (not unresolved free-vars)
;; End:

;;; init.el ends here
