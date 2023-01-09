;; -*- lexical-binding: t; -*-

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use `straight.el' for `use-package' expressions
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(straight-use-package 'org)
(setenv "PATH" (concat (getenv "PATH") ":/home/jonatan/.ghcup/bin"))

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

;; Ensure that environment variables are the same as the user’s shell
(when *is-a-mac*
  (use-package exec-path-from-shell
    :straight t
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
      (ns-raise-emacs))))

(defun bore/org-babel-tangle-config ()
  "Automatically tangle Emacs.org config when saving a file."
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'bore/org-babel-tangle-config)))

;; Some Garbage Collection Magic Hack
(use-package gcmh
  :straight t
  :config
  (setq gcmh-idle-delay 0.5  ; default is 15s
        gcmh-high-cons-threshold (* 16 1024 1024))  ; 16mb
  (gcmh-mode 1))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(use-package no-littering)

(setq user-full-name "Jonatan Borkowski"
      user-mail-address "jonatan.borkowski@pm.me")

(use-package emacs
  :straight nil
  :bind
  (("C-c t t" . modus-themes-toggle))
  :config
  (setq modus-themes-slanted-constructs t
	modus-themes-bold-constructs t
	modus-themes-mixed-fonts t
	modus-themes-scale-headings t
	modus-themes-subtle-line-numbers t
	modus-themes-inhibit-reload nil
	modus-themes-mode-line '(borderless)
	modus-themes-syntax '(faint)
	modus-themes-lang-checkers '(faint)
	modus-themes-completions '(opinionated)
	modus-themes-diffs 'desaturated)
  (load-theme 'modus-vivendi))

(use-package nyan-mode)
(nyan-mode 1)

(use-package ligature
  :straight (ligature :host github
                      :repo "mickeynp/ligature.el")
  :config
  ;; Enable all Recursive ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("==" "===" "!=" "!==" "=/=" "!!" "??"
                                       "%%" "&&" "&&&" "||" "|||" "=>" "->" "<-"
                                       "##" "###" "####" "//" "f\"" "f'" "${"
                                       "?." "?:" "/*" "*/" "///" "'''" "\"\"\""
                                       "```" "<!--" "-->" ">-" "-<" "::" ">>"
                                       ">>>" "<<" "<<<" "://" "++" "+++" "--"
                                       "---" "**" "***" "+=" "-=" "*=" "/=" "=~"
                                       "<*" "<*>" "<|" "|>" "<|>" "<$>" "<=>"
                                       "<>" "<+>" ">>-" "-<<" "__" "-[ ]" "-[x]"
                                       "\\b" "\\n" "\\r" "\\t" "\\v" "|=" "!~"
                                       "<<~" "<<=" ">>=" "=<<"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(defun bore/with-font-faces-mac ()
  "Setup all Emacs font faces."
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font (font-spec :family "Iosevka" :size 14 :weight 'regular))
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family "Iosevka" :size 14 :weight 'regular))
    (set-face-attribute 'variable-pitch nil :font (font-spec :family "Iosevka Aile" :size 14 :weight 'regular))))

(defun bore/with-font-faces-linux ()
  "Setup all Emacs font faces."
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font (font-spec :family "Iosevka Etoile" :size 20 :weight 'regular))
    (set-face-attribute 'fixed-pitch nil :font (font-spec :family "Iosevka Etoile" :size 20 :weight 'regular))
    (set-face-attribute 'variable-pitch nil :font (font-spec :family "Iosevka Aile" :size 20 :weight 'regular))))

(when *is-a-mac*
  (add-hook 'after-init-hook 'bore/with-font-faces-mac)
  (add-hook 'server-after-make-frame-hook 'bore/with-font-faces))


(when *is-a-linux*
  (add-hook 'after-init-hook 'bore/with-font-faces-linux)
  (add-hook 'server-after-make-frame-hook 'bore/with-font-linux))

;; Make those lambdas pretty again
(global-prettify-symbols-mode t)

;; For the first time remember to run M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; Happy people don't count numbers, they also have a small performance boost to Emacs
(setq display-line-numbers-type nil)

;; But for sure disable line numbers in some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                eshell-mode-hool))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package tab-bar
  :straight nil
  :config
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

(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

(setq mode-line-end-spaces nil)

;;(use-package power-mode
;;  :straight t)

(setq inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      kill-do-not-save-duplicates t
      require-final-newline t
      password-cache-expiry nil
      custom-safe-themes t
      scroll-margin 2
      ;; select-enable-clipboard t
      visible-bell t
      warning-minimum-level :error)

(recentf-mode 1)
(global-so-long-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(set-default-coding-systems 'utf-8)
(global-hl-line-mode 1)

(setq x-alt-keysym 'meta) ;; Alt as Meta key

(use-package emacs
  :straight nil
  :bind
  (("C-x K"   . bore/kill-buffer)
   ("C-z"     . repeat)
   ("C-c q q" . kill-emacs))
  :init

  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		  (replace-regexp-in-string
		   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		   crm-separator)
		  (car args))
	  (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Clean up whitespace, newlines and line breaks
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(defun bore/kill-buffer (&optional arg)
  "Kill buffer which is currently visible (ARG)."
  (interactive "P")
  (if arg
      (call-interactively 'kill-buffer)
    (kill-this-buffer)))

(use-package ibuffer
  :straight nil
  :bind (("C-x C-b" . ibuffer))
  :config
  (setq ibuffer-expert t
        ibuffer-display-summary nil
        ibuffer-use-other-window nil
        ibuffer-show-empty-filter-groups nil
        ibuffer-movement-cycle nil
        ibuffer-default-sorting-mode 'filename/process
        ibuffer-use-header-line t
        ibuffer-default-shrink-to-minimum-size nil
        ibuffer-formats
        '((mark modified read-only locked " "
                (name 40 40 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " "
                (name 16 -1)
                " " filename))
        ibuffer-saved-filter-groups nil
        ibuffer-old-time 48)
  (add-hook 'ibuffer-mode-hook #'hl-line-mode))

(use-package savehist
  :straight nil
  :config
  (setq savehist-save-minibuffer-history t
        savehist-autosave-interval nil
        savehist-additional-variables
        '(kill-ring
          register-alist
          mark-ring global-mark-ring
          search-ring regexp-search-ring))
  (savehist-mode 1))
(setq undo-limit 80000000
      history-limit 5000
      history-delete-duplicates t)

;; Enable autosave and backup
(setq auto-save-default t
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save" user-emacs-directory) t))
      make-backup-files t
      backup-directory-alist `((".*" . ,(expand-file-name "backup" user-emacs-directory)))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      create-lockfiles nil)

(use-package recentf
  :straight nil
  :commands recentf-open-files
  :config
  (setq recentf-max-menu-items 100
        recentf-max-saved-items 100)
  (recentf-mode 1))

(use-package paren
  :straight nil
  :config
  (setq show-paren-delay 0
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t)
  (show-paren-mode 1))

;; A little bit of rainbow here and there
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package elec-pair
  :straight nil
  :config
  (setq electric-pair-inhibit-predicate'electric-pair-conservative-inhibit
        electric-pair-skip-self 'electric-pair-default-skip-self
        electric-pair-skip-whitespace nil
        electric-pair-preserve-balance t)
  (electric-indent-mode 1)
  (electric-pair-mode 1))

(setq scroll-conservatively 101                    ; value greater than 100 gets rid of half page jumping
      mouse-wheel-scroll-amount '(3 ((shift) . 3)) ; how many lines at a time
      mouse-wheel-progressive-speed t              ; accelerate scrolling
      mouse-wheel-follow-mouse 't)                 ; scroll window under mouse

(use-package isearch
  :straight nil
  :bind
  :config
  (setq isearch-lazy-count t))

;; Just a thought... and you are there!
(use-package avy
  :straight t
  :bind (("C-'" . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-'" . avy-isearch))
  :config
  (setq avy-keys '(?n ?e ?i ?s ?t ?r ?i ?a)
        avy-timeout-seconds .3
        avy-background t))

(use-package ace-window
  :straight t
  :commands ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?n ?e ?i ?s ?t ?r ?i ?a)))

;; Allow me to undo my windows
(use-package winner
  :straight nil
  :hook
  (after-init . winner-mode))

(setq mode-line-position-line-format `(" %l:%c"))
(setq mode-line-position-column-line-format '(" %l,%c"))
(setq mode-line-compact nil)
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
  :straight t
  :config
  (minions-mode 1))

(use-package which-key
  :straight t
  :defer t
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))

(use-package helpful
  :straight t
  :commands helpful-callable helpful-variable helpful-command helpful-key
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key]      . helpful-key))

(use-package hl-todo
  :straight t
  :hook (prog-mode . hl-todo-mode))

(use-package iedit
  :straight t
  :commands iedit-mode iedit-rectangle-mode
  :bind ("C-;" . iedit-mode))

(use-package evil-multiedit
  :defer t)

(use-package multiple-cursors
  :straight t
  :bind (("C-<" . mc/mark-previous-like-this)
         ("C->" . mc/mark-next-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; add evil-mc

(use-package clipetty
  :straight t
  :unless (display-graphic-p)
  :hook (tty-setup . global-clipetty-mode))

(use-package ispell
  :straight nil
  :config
  (setq ispell-program-name "hunspell"
        ispell-dictionary "en_US,pl_PL")
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic ispell-dictionary))

(use-package flyspell
  :hook ((message-mode git-commit-mode org-mode text-mode) . flyspell-mode)
  :bind (:map flyspell-mode-map
              ("C-." . nil)
              ("C-;" . nil))
  :config
  (setq flyspell-issue-welcome-flag nil
        flyspell-issue-message-flag nil))

(use-package browse-url
  :straight nil
  :config
  (setq browse-url-secondary-browser-function 'eww-browse-url
        browse-url-browser-function 'browse-url-default-browser))

(use-package shr
  :straight nil
  :config
  (setq shr-use-colors nil             ; t is bad for accessibility
        shr-use-fonts nil              ; t is not for me
        shr-max-image-proportion 0.6
        shr-image-animate nil          ; No GIFs, thank you!
        shr-width nil
        shr-discard-aria-hidden t
        shr-cookie-policy nil))

(use-package url-cookie
  :straight nil
  :config (setq url-cookie-untrusted-urls '(".*")))

(use-package eww
  :straight nil
  :bind ("C-c o b" . eww)
  :config
  (setq eww-restore-desktop t
        eww-desktop-remove-duplicates t
        eww-header-line-format nil
        eww-search-prefix "https://duckduckgo.com/?ia="
        eww-download-directory (expand-file-name "~/Downloads")
        eww-suggest-uris
        '(eww-links-at-point
          thing-at-point-url-at-point)
        eww-history-limit 150
        eww-use-external-browser-for-content-type
        "\\`\\(video/\\|audio\\)"
        eww-browse-url-new-window-is-tab nil
        eww-form-checkbox-selected-symbol "[X]"
        eww-form-checkbox-symbol "[ ]"
        eww-retrieve-command nil))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode))

(use-package dired
  :straight nil
  :commands dired dired-jump
  :config
  (setq dired-kill-when-opening-new-dired-buffer t
	delete-by-moving-to-trash t
	dired-dwim-target t
	dired-recursive-copies 'always
	dired-recursive-deletes 'always))

(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
	 :map vertico-map
	 ("C-x C-d" . consult-dir)
	 ("C-x C-j" . consult-dir-jump-file)))

(use-package project
  :straight t)

(use-package magit
  :straight t
  :commands magit-file-delete
  :init
  (setq magit-auto-revert-mode nil)             ; `global-auto-revert-mode'
  :config
  (setq transient-default-level 5
        magit-diff-refine-hunk t                ; show granular diffs in selected hunk
        magit-save-repository-buffers nil       ; don't autosave repo buffers
        magit-revision-insert-related-refs nil) ; don't display parent/related refs in commit buffers
  :custom
  (magit-section-visibility-indicator nil)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package magit-todos
  :after magit
  :config
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?:?") ; make colon optional
  (define-key magit-todos-section-map "j" nil))


(use-package ediff
  :straight nil
  :config
  (setq ediff-merge-split-window-function 'split-window-horizontally
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package diff-hl
  :straight t
  :config
  (setq diff-hl-draw-borders nil)
  (global-diff-hl-mode 1))

(use-package eshell
  :straight nil
  :commands eshell
  :bind ("C-c o E" . eshell)
  :config
  (setq eshell-kill-processes-on-exit t
        eshell-highlight-prompt t
        eshell-hist-ignoredups t
        eshell-prompt-regexp "^.* λ "))

(use-package eshell-syntax-highlighting
  :straight t
  :after eshell-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(use-package eshell-toggle
  :straight t
  :commands eshell-toggle
  :bind ("C-c o e" . eshell-toggle)
  :custom
  (eshell-toggle-size-fraction 4)
  (eshell-toggle-run-command nil))

(use-package vterm
  :straight t
  :bind
  ("C-c o t" . vterm-other-window)
  ("C-c o T" . vterm)
  :config
  (setq vterm-kill-buffer-on-exit t
        vterm-always-compile-module t
        vterm-max-scrollback 5000
        vterm-timer-delay nil
        vterm-shell "/bin/zsh"))

(use-package rainbow-mode
  :straight t
  :commands rainbow-mode)

(use-package olivetti
  :straight t
  :commands olivetti-mode
  :config
  (setq olivetti-body-width 100))

(setq xterm-set-window-title t)
(setq visible-cursor nil)
;; Enable the mouse in terminal Emacs
(add-hook 'tty-setup-hook #'xterm-mouse-mode)

(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	     '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
	       nil
	       (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs
        (append yas-snippet-dirs
                '("~/.emacs.d/snippets")))
  (yas-global-mode 1)
  )

;; Configure Tempel
(use-package tempel
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
	 ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'. `tempel-expand'
    ;; only triggers on exact matches. Alternatively use `tempel-complete' if
    ;; you want to see all matches, but then Tempel will probably trigger too
    ;; often when you don't expect it.
    ;; NOTE: We add `tempel-expand' *before* the main programming mode Capf,
    ;; such that it will be tried first.
    (setq-local completion-at-point-functions
		(cons #'tempel-expand
		      completion-at-point-functions)))

  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook #'tempel-abbrev-mode)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (tempel-global-abbrev-mode)
  )

(use-package elfeed
  :straight t
  :bind
  ("C-x w" . elfeed))

(use-package elfeed-org
  :straight t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/org/elfeed.org")))

;; Enable vertigo
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  (setq vertico-resize t
	vertico-cycle t
	vertico-count 17)

  (setq completion-in-region-function
	(lambda (&rest args)
	  (apply (if vertico-mode
		     #'consult-completion-in-region
		   #'completion--in-region)
		 args))))

;; Use the orderless completion style
(use-package orderless
  :straight t
  :init

  (setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion))))))

;; I want to know every detail... one the margin
(use-package marginalia
  :after vertico
  :straight t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy
                           marginalia-annotators-light
                           nil))
  :init
  (marginalia-mode))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
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
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :straight t
  :defer t
  :bind (
	 ;; C-x bindings (ctl-x-map)
	 ("C-x C-r" . consult-recent-file)
	 ("C-x M-:" . consult-complex-command)     ; orig. repeat-complex-command
	 ("C-x b"   . consult-buffer)              ; orig. switch-to-buffer
	 ("C-x M-k" . consult-kmacro)
	 ("C-x M-m" . consult-minor-mode-menu)
	 ("C-x r b" . consult-bookmark)            ; override bookmark-jump
	 ("C-x 4 b" . consult-buffer-other-window) ; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame
	 ;; Other custom bindings
	 ("M-y" . consult-yank-pop)                ; orig. yank-pop
	 ("<help> a" . consult-apropos)            ; orig. apropos-command
	 ;; M-g bindings (goto-map)
	 ("M-g e" . consult-compile-error)
	 ("M-g f" . consult-flycheck)               ; Alternative: consult-flycheck
	 ("M-g g" . consult-goto-line)             ; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ; orig. goto-line
	 ("M-g o" . consult-outline)               ; Alternative: consult-org-heading
	 ("M-g m" . consult-mark)
	 ("M-g k" . consult-global-mark)
	 ("M-g i" . consult-imenu)
	 ("M-g I" . consult-imenu-multi)
	 ;; M-s bindings (search-map)
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
	 ;; Isearch integration
	 ("M-s e" . consult-isearch-history)
	 :map isearch-mode-map
	 ("M-e" . consult-isearch-history)          ; orig. isearch-edit-string
	 ("M-s e" . consult-isearch-history))       ; orig. isearch-edit-string

  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0
	register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.5 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-project-buffer consult--source-bookmark
   :preview-key (kbd "M-."))

  (setq consult-narrow-key "<"
	consult-line-numbers-widen t
	consult-async-min-input 2
	consult-async-refresh-delay  0.15
	consult-async-input-throttle 0.2
	consult-async-input-debounce 0.1)

  (setq consult-project-root-function
	(lambda ()
	  (when-let (project (project-current))
	    (car (project-roots project))))))

(when *is-a-linux*
  (use-package mu4e
    :straight nil
    :commands mu4e mu4e-compose-new
    :bind ("C-c o m" . mu4e)

    :config
    (require 'mu4e-org)

    (setq mu4e-contexts
          (list
           (make-mu4e-context
            :name "Proton"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Proton" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "jonatan.borkowski@pm.me")
                    (user-full-name . "Jonatan Borkowski")
                    (user-draft-folder . "/Proton/Drafts")
                    (mu4e-sent-folder  . "/Proton/Sent Mail")
                    (mu4e-refile-folder  . "/Proton/All Mail")
                    (mu4e-trash-folder  . "/Proton/Trash")))

           ;; Work account
           (make-mu4e-context
            :name "Work"
            :match-func
            (lambda (msg)
              (when msg
                (string-prefix-p "/Restamatic" (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "jonatan.borkowski@restaumatic.com")
                    (user-full-name    . "Jonatan Borkowski")
                    (mu4e-drafts-folder  . "/Restaumatic/[Gmail]/Drafts")
                    (mu4e-sent-folder  . "/Restaumatic/[Gmail]/Sent Mail")
                    (mu4e-refile-folder  . "/Restaumatic/[Gmail]/All Mail")
                    (mu4e-trash-folder  . "/Restamatic/[Gmail]/Trash")))))

    ;; Get mail
    (setq mu4e-maildir "~/.mail"
          mu4e-get-mail-command "mbsync -a"
          mu4e-change-filenames-when-moving t   ; needed for mbsync
          mu4e-update-interval 120)             ; update every 2 minutes

    ;; Send mail
    (setq mail-specify-envelope-from t
          message-send-mail-function 'smtpmail-send-it
          smtpmail-auth-credentials "~/.authinfo.gpg"
          smtpmail-smtp-server "127.0.0.1"
          message-kill-buffer-on-exit t
          smtpmail-stream-type 'starttls
          smtpmail-smtp-service 1025))

  ;; Trust certificates
  (require 'gnutls)
  (if (file-exists-p "~/.config/protonmail/bridge/cert.pem")
      (add-to-list 'gnutls-trustfiles (expand-file-name "~/.config/protonmail/bridge/cert.pem")))
  )


(when *is-a-linux*
  (use-package org-msg
    :after mu4e
    :straight t
    :config
    (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil tex:dvipng"
          org-msg-startup "hidestars indent inlineimages"
          org-msg-greeting-name-limit 3
          org-msg-default-alternatives '((new . (utf-8 html))
                                         (reply-to-text . (utf-8))
                                         (reply-to-html . (utf-8 html)))
          org-msg-convert-citation t)))

(use-package org-jira
  :straight t
  :init
  (make-directory "~/.org-jira" 0)
  :config

  (setq jiralib-url "https://restaumatic.atlassian.net")

  (setq org-jira-custom-jqls
        '((:jql " project IN (RS) and createdDate >= '2022-01-01' order by created DESC "
                :limit 10
                :filename "this-years-work")
          (:jql " project IN (RS)
AND status IN ('To Do', 'In Development')
AND (labels = EMPTY or labels NOT IN ('FutureUpdate'))
order by priority, created DESC "
                :limit 20
                :filename "ex-ahu-priority-items")
          ))
  )

(use-package envrc
  :straight t
  :config
  (envrc-global-mode))

(defun bore/project-override (dir)
  (let ((override (locate-dominating-file dir ".project.el")))
    (if override
        (cons 'vc override)
      nil)))
(use-package project
  :config
  (add-hook 'project-find-functions #'bore/project-override))

(use-package lsp-mode
    :straight t

    :hook ((c-mode
	    c++-mode
	    c-or-c++-mode
	    js-mode
	    rust-mode
	    typescript-mode
	    purescript-mode
	    haskell-mode
	    elixir-mode) . lsp-deferred)
    :bind (:map lsp-mode-map
		("C-c c d" . lsp-describe-thing-at-point)
		("C-c c s" . consult-lsp-symbols)
		("C-c c t" . lsp-goto-type-definition)
		("M-," . lsp-find-references)
		("M-." . lsp-find-definition)
		("C-c c f" . lsp-format-buffer)
		("C-c c x" . lsp-execute-code-action)
		("C-c c r" . lsp-rename)
		("C-c c j" . consult-lsp-symbols))
    :commands lsp lsp-deferred

    :config
    (setq lsp-idle-delay 0.5
	  lsp-diagnostics-provider t
	  lsp-keep-workspace-alive nil
	  lsp-headerline-breadcrumb-enable nil
	  lsp-modeline-code-actions-enable nil
	  lsp-modeline-diagnostics-enable nil
	  lsp-modeline-workspace-status-enable nil
	  lsp-enable-file-watchers nil
	  lsp-file-watch-threshold 5000
	  read-process-output-max (* 1024 1024)
	  lsp-log-io t))


    (add-hook 'lsp-completion-mode-hook
	      (lambda ()
		(setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (orderless))))))

;;  (add-to-list 'tramp-remote-path "~/.ghcup/bin")
;;  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  (use-package lsp-haskell
    :straight t
    :after (lsp haskell-mode))

  (use-package consult-lsp
    :straight t
    :after lsp-mode)

(use-package corfu
  ;; Optional customizations
  :straight t
  :custom
  (corfu-cycle t)                ; enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ; enable auto completion
  (corfu-quit-no-match t)        ; automatically quit if there is no match
  (corfu-echo-documentation nil) ; do not show documentation in the echo area
   ;; This is recommended since Dabbrev can be used globally (M-/).
  :init
  (global-corfu-mode)
  )

(use-package cape
  :straight t
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :config
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent))

;; Use the overpowered expand of the hippies
(use-package hippie-exp
  :straight nil
  :bind ("M-/" . hippie-expand)
  :config
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev-visible
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-expand-line)))

(use-package flymake
  :straight nil
  :hook (prog-mode . flymake-mode)
  :bind (("M-n" . flymake-goto-next-error)
	 ("M-p"  . flymake-goto-prev-error))
  :config
  (setq flymake-suppress-zero-counters t)
  (setq flymake-mode-line-counter-format
	'(" " flymake-mode-line-error-counter
	  flymake-mode-line-warning-counter
	  flymake-mode-line-note-counter "")))

(use-package flymake-collection
  :straight t
  :hook (after-init . flymake-collection-hook-setup))

(use-package flymake-grammarly
    :straight t)
(add-hook 'text-mode-hook 'flymake-grammarly-load)
(add-hook 'latex-mode-hook 'flymake-grammarly-load)
(add-hook 'org-mode-hook 'flymake-grammarly-load)
(add-hook 'markdown-mode-hook 'flymake-grammarly-load)
(add-hook 'magit-message 'flymake-grammarly-load)

(use-package reformatter
  :straight t
  :defer t)

(use-package agda2-mode
  :straight t
  :mode (("\\.agda\\'" . agda2-mode)
	 ("\\.lagda.md\\'" . agda2-mode)))

(use-package agda-input
  :straight (:package "agda-input" :type git :host github :repo "agda/agda" :files ("src/data/emacs-mode/agda-input.el")))

(use-package idris2-mode
  :straight (:package "idris2-mode" :type git :host github :repo "idris-community/idris2-mode"))

(use-package haskell-mode
  :straight t
  :mode (("\\.hs\\'"    . haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))

  :hook ((haskell-mode . interactive-haskell-mode)
         (haskell-mode . haskell-indentation-mode)
         (haskell-mode . fourmolu-format-on-save-mode))

  :bind (:map haskell-mode-map
              ("C-c c o" . hoogle)
              ("C-c c f" . fourmolu-format-buffer))
  :custom
  (haskell-interactive-popup-errors nil)
  (haskell-process-log t)
  (haskell-process-type 'stack-ghci)
  (haskell-process-load-or-reload-prompt t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-suggest-hoogle-imports t)
  (haskell-process-suggest-remove-import-lines t))

(reformatter-define fourmolu-format
  :program "fourmolu"
  :args (list "--stdin-input-file" (buffer-file-name))
  :lighter " fourmolu")

(use-package ghcid
  :straight (:package "ghcid" :host nil :type git :repo "https://github.com/ndmitchell/ghcid" )
  :defer
  :load-path "site-lisp/"
  :bind (:map projectile-mode-map
              ("C-c m s" . ghcid)
              ("C-c m b" . show-ghcid-buf)
              ("C-c m t" . set-ghcid-target))
  :custom
  (ghcid-target "")
  ;;:config (setq-local default-directory projectile-project-root)
  :preface
  (use-package haskell-mode :ensure t)
  (defun show-ghcid-buf ()
    (interactive)
    (show-buffer ghcid-buf-name))
  (defun set-ghcid-target (ghcid-targ &optional ghcid-test-targ)
    (interactive
     (list
      (completing-read "ghcid target: " (map 'list (lambda (targ) (format "%s:%s" (projectile-project-name) targ)) (haskell-cabal-enum-targets)))
      (completing-read "ghcid --test target: " '("--test=main" "--test=Main.main" nil))))
    (setq ghcid-target ghcid-targ)
    (when ghcid-test-targ
      (setq ghcid-target-test (format "%s" ghcid-test-targ)))
    (kill-ghcid)
    (ghcid)))

(use-package toml-mode
  :straight t
  :mode "\\.toml\\'")

(use-package yaml-mode
  :straight t
  :mode "\\.ya?ml\\'")
(setq js-indent-level 2)

(use-package dhall-mode
  :defer t
  :config
  (set-repl-handler! 'dhall-mode #'dhall-repl-show)
  (setq dhall-format-at-save t)
  )

(use-package lua-mode
  :straight t
  :mode "\\.lua\\'")

(use-package markdown-mode
  :straight t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :commands (markdown-mode gfm-mode)
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package org
  :straight t
  :commands org-capture org-agenda
  :init
  (add-hook 'org-mode-hook
	    (lambda ()
	      (variable-pitch-mode 1)
	      (org-modern-mode)
	      (visual-line-mode 1)))
  :config
  (setq org-directory "~/org/"
	org-adapt-indentation nil
	org-edit-src-persistent-message nil
	org-fold-catch-invisible-edits 'show-and-error
	org-insert-heading-respect-content t
	org-fontify-quote-and-verse-blocks t
	org-tags-column 0
	org-hide-emphasis-markers t
	org-hide-macro-markers t
	org-hide-leading-stars nil
	org-ellipsis "…"
	org-capture-bookmark nil
	org-mouse-1-follows-link t
	org-pretty-entities t
	org-pretty-entities-include-sub-superscripts nil
	org-indirect-buffer-display 'current-window
	org-eldoc-breadcrumb-separator " → "
	org-enforce-todo-dependencies t
	org-startup-folded t
	org-use-sub-superscripts '{}
	org-src-fontify-natively t
	org-src-tab-acts-natively t
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t
	org-fontify-whole-heading-line t
	org-capture-bookmark nil
	org-priority-faces
	'((?A . error)
	  (?B . warning)
	  (?C . success))

	org-entities-user
	'(("flat"  "\\flat" nil "" "" "266D" "♭")
	  ("sharp" "\\sharp" nil "" "" "266F" "♯"))

	org-imenu-depth 6
	org-structure-template-alist
	'(("s" . "src")
	  ("e" . "src emacs-lisp")
	  ("h" . "src haskell")
	  ("E" . "example")
	  ("q" . "quote")
	  ("c" . "comment")))      )

(use-package org-modern
  :straight t
  :config
  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.2)
  (dolist (face '((org-level-1 . 1.15)
		  (org-level-2 . 1.10)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
  (set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'medium :height (cdr face))
  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.2)
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)))

(use-package denote
  :straight t
  :bind
  (("C-c n n" . denote)
   ("C-c n i" . denote-link)
   ("C-c n b" . denote-link-backlinks)
   ("C-c n l" . denote-link-find-file)
   ("C-c n r" . denote-rename-file)
   ("C-c n j" . bore/journal)
   ("C-c n f" . consult-notes))
  :config

  (setq denote-directory (expand-file-name "~/org/notes/")
	denote-known-keywords '("linux" "journal" "emacs" "embedded" "hobby")
	denote-infer-keywords t
	denote-sort-keywords t
	denote-prompt '(title keywords)
	denote-front-matter-date-format 'org-timestamp
	denote-templates '((todo . "* Tasks:\n\n"))))

(defun bore/journal ()
  "Create an entry tagged 'journal' with the date as its title"
  (interactive)
  (denote
   (format-time-string "%A %e %B %Y")
   '("journal")))

(use-package consult-notes
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
	     consult-notes-search-in-all-notes
	     consult-notes-org-roam-find-node
	     consult-notes-org-roam-find-node-relation)
  :config
  (setq consult-notes-file-dir-sources
	`(("Notes" ?n "~/org/notes")
	  ("Roam"  ?r "~/org/roam")))
  (when (locate-library "denote")
    (consult-notes-denote-mode)))

(use-package org-agenda
  :straight nil
  :bind
  (("C-c a" . org-agenda)
   ("C-c x" . org-capture))
  :init
  (add-hook 'org-agenda-finalize-hook (lambda () (org-modern-agenda)))
  :config
  (setq-default org-agenda-files (list org-directory)
                org-agenda-compact-blocks nil
                org-agenda-window-setup 'current-window
                org-agenda-skip-unavailable-files t
                org-agenda-span 10
                calendar-week-start-day 1
                org-agenda-start-on-weekday nil
                org-agenda-start-day "-3d"
                org-agenda-deadline-faces
                '((1.001 . error)
                  (1.0 . org-warning)
                  (0.5 . org-upcoming-deadline)
                  (0.0 . org-upcoming-distant-deadline))
                org-agenda-inhibit-startup t))

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/inbox.org" "True Life Tasks")
	 "* TODO %? \n%U" :empty-lines 1)
	("w" "Todo (work)" entry (file+headline "~/org/inbox.org" "Work Tasks")
	 "* TODO %? \n%U" :empty-lines 1)
	("e" "Event" entry (file+headline "~/org/agenda.org" "Agenda")
	 "** %? \n %^T\n%U" :empty-lines 1)))

(use-package org-cliplink
  :straight t
  :config
  (global-set-key (kbd "C-c l") 'org-cliplink))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((scheme . t)
   (emacs-lisp . t)))

(when *is-a-mac*
  (use-package orgmark
	:straight (orgmark :host github
			   :repo "casouri/OrgMark")))

(setq js-indent-level 2
      typescript-indent-level 2
      json-reformat:indent-width 2
      css-indent-offset 2)

(use-package terraform-mode
  :straight t)

(use-package rustic
  :straight t
  :bind (:map rustic-mode-map
              ("C-c c a" . lsp-rust-analyzer-status)
              ("C-c c b" . rustic-cargo-build))
  :config
  (setq lsp-eldoc-hook nil)
  (setq rust-format-on-save t))

(use-package docker
  :straight t)
(use-package dockerfile-mode
  :straight t)

(use-package js2-mode
  :straight t
  :mode "\\.jsx?\\'"
  ;; Set up proper indentation in JavaScript and JSON files
  :hook (js2-mode . prettier-format-on-save-mode)
  :init (setq-default js-indent-level 2)
  :bind (:map js2-mode-map
              ("C-c C-f"  . prettier-format-buffer))
  :config
  ;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil)

  (setq js--prettify-symbols-alist nil  ; I will handle ligatures by myself
        js2-highlight-level 3))         ; More highlighting

(reformatter-define prettier-format
  :program "prettier"
  :args (list "--stdin-filepath" (buffer-file-name))
  :lighter " prettier")

(use-package typescript-mode
  :straight t
  :mode "\\.ts\\'"
  :hook (typescript-mode . prettier-format-on-save-mode)
  :bind (:map typescript-mode-map
              ("C-c C-f"  . prettier-format-buffer))
  :config (setq typescript-indent-level 2))

(use-package purescript-mode
  :straight t
  :mode "\\.purs\\'"
  :hook ((purescript-mode . turn-on-purescript-indentation)
         (purescript-mode . purs-tidy-format-on-save-mode))
  :bind (:map purescript-mode-map
              ("C-c c f"  . purs-tidy-format-buffer)))

(use-package geiser-mit
  :straight t)

;;; init.el ends here
