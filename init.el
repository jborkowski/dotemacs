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

(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

;; Ensure that environment variables are the same as the user’s shell
(when *is-a-mac*
  (use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns x))
  :hook (after-init . exec-path-from-shell-initialize)))

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

;; Use Nord Theme
(use-package nord-theme)
(load-theme 'nord t)

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

(defun bore/with-font-faces ()
  "Setup all Emacs font faces."
  (when (display-graphic-p)
    (set-face-attribute 'default nil :font (font-spec :family "Liga SFMono Nerd Font" :size 16 :weight 'regular))
      (set-face-attribute 'fixed-pitch nil :font (font-spec :family "Liga SFMono Nerd Font" :size 16 :weight 'regular))
      (set-face-attribute 'variable-pitch nil :font (font-spec :family "Liga SFMono Nerd Font" :size 16 :weight 'light))))

(add-hook 'after-init-hook 'bore/with-font-faces)
(add-hook 'server-after-make-frame-hook 'bore/with-font-faces)

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

(use-package emacs-everywhere
  :straight t)

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(setq inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message nil
      kill-do-not-save-duplicates t
      require-final-newline t
      password-cache-expiry nil
      custom-safe-themes t
      scroll-margin 2
      select-enable-clipboard t
      visible-bell t
      warning-minimum-level :error)

(global-so-long-mode 1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(set-default-coding-systems 'utf-8)
(global-hl-line-mode 1)

(use-package undo-tree)
(global-undo-tree-mode 1)

(use-package emacs
      :straight nil
      :bind
      (("C-x K"   . bore/kill-buffer)
       ("C-z"     . repeat)
       ("C-c q q" . kill-emacs))
      :init
      ;; Add prompt indicator to `completing-read-multiple'.
      ;; Alternatively try `consult-completing-read-multiple'.
      (defun crm-indicator (args)
        (cons (concat "[CRM] " (car args)) (cdr args)))
      (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

      ;; TAB cycle if there are only few candidates
      (setq completion-cycle-threshold 3)

      ;; Do not allow the cursor in the minibuffer prompt
      (setq minibuffer-prompt-properties
            '(read-only t cursor-intangible t face minibuffer-prompt))
      (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

      ;; Clean up whitespace, newlines and line breaks
      (add-hook 'before-save-hook 'delete-trailing-whitespace)

      ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
      ;; Vertico commands are hidden in normal buffers.
      (setq read-extended-command-predicate
            #'command-completion-default-include-p)

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

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :straight nil
  :config
  (setq savehist-save-minibuffer-history t
        history-length 1000
        history-delete-duplicates t
        savehist-autosave-interval nil     ; save on kill only
        savehist-additional-variables
        '(kill-ring                        ; persist clipboard
          register-alist                   ; persist macros
          mark-ring global-mark-ring       ; persist marks
          search-ring regexp-search-ring)) ; persist searches
  (savehist-mode 1))

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

;; Pretty minimal modeline that suits my mood
(setq mode-line-position-line-format `(" %l:%c"))
(setq mode-line-position-column-line-format '(" %l,%c"))       ; Emacs 28
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

;; Hide modeline "lighters"
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
        eww-search-prefix "https://www.google.com/search?q="
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

;; Enable vertigo
(use-package vertico
  :straight t
  :init
  (vertico-mode)
  (setq vertico-resize t
	vertico-cycle t
	vertico-count 17
	completion-in-region-function
	(lambda (&rest args)
	  (apply (if vertico-mode
		     #'consult-completion-in-region
		   #'completion--in-region)
		 args))))

;; Use the orderless completion style
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

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
               :config
               (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

( use-package consult
  :straight t
  :defer t
  :bind (;; C-x bindings (ctl-x-map)
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
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)
  (setq xref-show-xrefs-function #'consult-xref
	xref-show-definitions-function #'consult-xref)

  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.5 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
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

(use-package envrc
  :straight t
  :config
  (envrc-global-mode))

(use-package tramp
    :straight nil
    :config
    (setq tramp-default-method "ssh"))

;; (require 'tramp-cache)

;; (defconst docker-tramp-method "docker")
;; (defconst docker-tramp-docker-exec "docker")

;; (defun docker-tramp-add-method ()
;;   "Add docker tramp method."
;;   (add-to-list 'tramp-methods
;;                `(,docker-tramp-method
;;                  (tramp-login-program      ,docker-tramp-docker-exec)
;;                  (tramp-login-args         (("exec" "-it") ("-u" "%u") ("%h") ("sh")))
;;                  (tramp-remote-shell       "/bin/zsh")
;;                  (tramp-remote-shell-args  ("-i" "-c")))))

;; (defun docker-trump--running-containers ()
;;   "Returns list with container names."
;;   (split-string (shell-command-to-string "docker ps --format '{{.Names}}:'") "\n"))

;; (defconst docker-tramp-completion-function-alis
;;     '(docker-trump--running-containers))


;; (eval-after-load 'tramp
;;   '(progn
;;      (docker-tramp-add-method)
;;      (tramp-set-completion-function docker-tramp-method docker-tramp-completion-function-alist)))


;; ;; TRAMP
;; (setq tramp-default-method "ssh")
;; (add-to-list 'tramp-remote-path "/home/user/.ghcup/bin:/home/user/.cabal/bin:/home/user/.local/bin")

;; ;; tramp minibuffer
;; (add-to-list 'directory-abbrev-alist
;;              '("^/redo" . "/ssh:re|docker:user@devcontainer_dev_1:"))

;; (add-to-list 'tramp-connection-properties
;;              (list (regexp-quote "/ssh:re:")
;;                    "direct-async-process" t
;;                    "remote-shell" "/bin/zsh"))

;; (add-hook 'find-file-hook
;;           (lambda ()
;;             (if (file-remote-p default-directory)
;;               (setq-local projectile-mode-line "Projectile"))))


;; (add-to-list 'directory-abbrev-alist
;;              '("^/redo" . "/ssh:re|docker:user@devcontainer_dev_1:"))

;; (setq remote-file-name-inhibit-cache nil)

;; ;; Disable version control to avoid delays
;; (setq vc-ignore-dir-regexp
;;       (format "%s\\|%s"
;;               vc-ignore-dir-regexp
;;               tramp-file-name-regexp))

;; (setq tramp-verbose 1)

;; (defun go-local ()
;;   "Destroy all TRAMP connections and kill all associated
;; buffers. Be aware that this will destroy local sudo/root TRAMP
;; sessions."
;;   (interactive)
;;   (ignore-errors (tramp-cleanup-all-connections))
;;   (ignore-errors (tramp-cleanup-all-buffers)))




;; (use-package dired-rsync
;;   :ensure t
;;   :config
;;   (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

(defun bore/project-override (dir)
    (let ((override (locate-dominating-file dir ".project.el")))
      (if override
        (cons 'vc override)
        nil)))
(use-package project
  :config
  (add-hook 'project-find-functions #'bore/project-override))

;; Going cxloser to the sun.. I mean Emacs
(use-package eglot
  :straight t
  :commands eglot eglot-ensure
  :hook (haskell-mode . eglot-ensure)
  :bind (:map eglot-mode-map
              ("C-c c j" . consult-eglot-symbols)
              ("C-c c x" . consult-flymake)
              ("C-c c a" . eglot-code-actions)
              ("C-c c r" . eglot-rename)
              ("C-c c f" . eglot-format)
              ("C-c c d" . eldoc))
  :config
  (setq eglot-sync-connect 1
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-confirm-server-initiated-edits nil
        eldoc-echo-area-display-truncation-message nil
        eldoc-echo-area-use-multiline-p 3)
  (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server" "--lsp"))))

(use-package consult-eglot
  :straight t
  :after eglot)

(use-package dumb-jump
  :straight t
  :defer t)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(use-package corfu
  ;; Optional customizations
  :straight t
  :custom
  (corfu-cycle t)                ; enable cycling for `corfu-next/previous'
  (corfu-auto nil)               ; disable auto completion
  (corfu-quit-no-match t)        ; automatically quit if there is no match
  (corfu-echo-documentation nil) ; do not show documentation in the echo area
  :init
  (corfu-global-mode))

(use-package cape
  :straight t
  :after corfu
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

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

(use-package haskell-mode
  :straight t
  :mode (("\\.hs\\'"    . haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :hook (haskell-mode . interactive-haskell-mode)
  :bind (:map haskell-mode-map
              ("C-c C-o" . hoogle)
              ("C-c C-h" . hs-lint))
  :custom
  (setq haskell-interactive-popup-errors nil)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-to-list 'completion-ignored-extensions ".hi")
  (haskell-interactive-popup-errors nil)
  (haskell-process-log t)
  (haskell-process-type 'cabal-new-repl)
  (haskell-process-load-or-reload-prompt t)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-suggest-remove-import-lines t))

(custom-set-variables '(haskell-stylish-on-save t))
(use-package hindent
  :straight t
  :hook (haskell-mode . hindent-mode))

(use-package toml-mode
  :straight t
  :mode "\\.toml\\'")

(use-package yaml-mode
  :straight t
  :mode "\\.ya?ml\\'")

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
              (org-indent-mode)
              (variable-pitch-mode 1)
              (visual-line-mode 1)
              (local-unset-key (kbd "C-'"))))

  :config
  (setq org-directory "~/org/"
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-hide-emphasis-markers t
        org-hide-leading-stars t
        org-capture-bookmark nil

        org-indirect-buffer-display 'current-window
        org-eldoc-breadcrumb-separator " → "
        org-enforce-todo-dependencies t
        org-entities-user
        '(("flat"  "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-image-actual-width nil
        org-imenu-depth 6
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))
        org-startup-indented t
        org-tags-column 0
        org-use-sub-superscripts '{}
        org-structure-template-alist
        '(("s" . "src")
          ("e" . "src emacs-lisp")
          ("h" . "src haskell")
          ("E" . "example")
          ("q" . "quote")
          ("c" . "comment")))
  )
(setq org-refile-targets
      '((nil :maxlevel . 3)
        (org-agenda-files :maxlevel . 3))
      ;; Without this, completers like ivy/helm are only given the first level of
      ;; each outline candidates. i.e. all the candidates under the "Tasks" heading
      ;; are just "Tasks/". This is unhelpful. We want the full path to each refile
      ;; target! e.g. FILE/Tasks/heading/subheading
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-todo-keywords
      '((sequence
         "TODO(t)"  ; A task that needs doing & is ready to do
         "STRT(s)"  ; A task that is in progress
         "WAIT(w)"  ; Something external is holding up this task
         "HOLD(h)"  ; This task is paused/on hold because of me
         "IDEA(i)"  ; An unconfirmed and unapproved task or notion
         "|"
         "DONE(d)"  ; Task successfully completed
         "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
        (sequence
         "[ ](T)"   ; A task that needs doing
         "[-](S)"   ; Task is in progress
         "[?](W)"   ; Task is being held up or paused
         "|"
         "[X](D)")  ; Task was completed
        (sequence
         "|"
         "OKAY(o)"
         "YES(y)"
         "NO(n)"))
      org-todo-keyword-faces
      '(("[-]"  . +org-todo-active)
        ("STRT" . +org-todo-active)
        ("[?]"  . +org-todo-onhold)
        ("WAIT" . +org-todo-onhold)
        ("HOLD" . +org-todo-onhold)
        ("NO"   . +org-todo-cancel)
        ("KILL" . +org-todo-cancel))
      )

(use-package org-agenda
  :straight nil
  :bind
  (("C-c a" . org-agenda)
   ("C-c x" . org-capture))
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
      '(
        ("t" "Todo" entry (file+headline "~/org/inbox.org" "Tasks")
         "* TODO %? \n%U" :empty-lines 1)
        ("e" "Event" entry (file+headline "~/org/agenda.org" "Agenda")
         "** %? \n %^T\n%U" :empty-lines 1))
      )

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/roam") ;; move my roam files to ~/org/roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n g" . org-roam-graph)
         :map org-mode-map
         ("C-M-i"    . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ; ensure the keymap is available
  (org-roam-db-autosync-mode))

(use-package org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nix-drv-mode
  :straight (nix-drv-mode
             :host github
             :repo "NixOS/nix-mode")
  :mode "\\.drv\\'")

(use-package nix-update
  :commands nix-update-fetch)

;;; init.el ends here
