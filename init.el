(setq user-emacs-directory "~/.own.emacs.d/")

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

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(setq inhibit-splash-screen t              ; disable splash sceen
      inhibit-startup-screen t             ; disable startup sceen
      inhibit-startup-message t            ; disable startup messame
      initial-scratch-message nil          ; disable startup message - buffer
      kill-do-not-save-duplicates t        ; kill those nasty duplicates
      require-final-newline t              ; remain POSIX compliant
      password-cache-expiry nil            ; I really want to trust my computer
      custom-safe-themes t                 ; all themes are safe, right?
      scroll-margin 2                      ; it's nice to maintain a little margin
      select-enable-clipboard t            ; integrate system's and Emacs' clipboard
      warning-minimum-level :error)        ; do not tell me about warnings

(global-so-long-mode 1)                    ; so long baby!
(fset 'yes-or-no-p 'y-or-n-p)              ; short questions answers
(global-subword-mode 1)                    ; iterate through CamelCase words
(global-auto-revert-mode t)                ; revert those buffers
(set-default-coding-systems 'utf-8)        ; default to utf-8 encoding
(global-hl-line-mode 1)                    ; enable global highlighting

(use-package undo-tree)
(global-undo-tree-mode 1)

(use-package general
  :config
  (general-create-definer bore/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (bore/leader-keys
    "t"  '(:ignore t :which-key "theme")
    "tt" '(consult-theme :which-key "choose theme")))

(defvar evil-want-C-g-bindings t)
(defvar evil-want-C-i-jump nil)  ; we do this ourselves
(defvar evil-want-C-u-scroll t)  ; moved the universal arg to <leader> u
(defvar evil-want-C-u-delete t)
(defvar evil-want-C-w-scroll t)
(defvar evil-want-C-w-delete t)
(defvar evil-want-Y-yank-to-eol t)
(defvar evil-want-abbrev-expand-on-insert-exit nil)
(defvar evil-respect-visual-line-mode nil)
  ;; enable global evil
  (use-package evil
    :straight t
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
					  ;:hooko (evil-mode . )
    :preface
    (setq evil-ex-search-vim-style-regexp t
	  evil-ex-visual-char-range t  ; column range for ex commands
	  evil-mode-line-format 'nil
	  ;; more vim-like behavior
	  evil-symbol-word-search t
	  ;; if the current state is obvious from the cursor's color/shape, then
	  ;; we won't need superfluous indicators to do it instead.
	  evil-default-cursor '+evil-default-cursor-fn
	  evil-normal-state-cursor 'box
	  evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
	  evil-insert-state-cursor 'bar
	  evil-visual-state-cursor 'hollow
	  ;; Only do highlighting in selected window so that Emacs has less work
	  ;; to do highlighting them all.
	  evil-ex-interactive-search-highlight 'selected-window
	  ;; It's infuriating that innocuous "beginning of line" or "end of line"
	  ;; errors will abort macros, so suppress them:
	  evil-kbd-macro-suppress-motion-error t
	  evil-undo-system 'undo-tree );; or 'undo-redo

    :config
    (evil-mode 1)
    (evil-select-search-module 'evil-search-module 'evil-search)

    ;; stop copying each visual state move to the clipboard:
    ;; https://github.com/emacs-evil/evil/issues/336
    ;; grokked from:
    ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
    (advice-add #'evil-visual-update-x-selection :override #'ignore)
    (advice-add #'help-with-tutorial :after (lambda (&rest _) (evil-emacs-state +1)))
    (defun +evil-default-cursor-fn ()
      (evil-set-cursor-color (get 'cursor 'evil-normal-color)))
    (defun +evil-emacs-cursor-fn ()
      (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))
    ;; Ensure `evil-shift-width' always matches `tab-width'; evil does not police
    ;; this itself, so we must.
    ;;    (setq-hook! 'after-change-major-mode-hook evil-shift-width tab-width)


    )

  (use-package evil-nerd-commenter
    :commands (evilnc-comment-operator
	       evilnc-inner-comment
	       evilnc-outer-commenter)
    :config ([remap comment-line] #'evilnc-comment-or-uncomment-lines))


  (use-package evil-snipe
    :commands evil-snipe-local-mode evil-snipe-override-local-mode
    :init
    (setq evil-snipe-smart-case t
	  evil-snipe-scope 'line
	  evil-snipe-repeat-scope 'visible
	  evil-snipe-char-fold t)
    :config
    (pushnew! evil-snipe-disabled-modes 'Info-mode 'calc-mode 'treemacs-mode 'dired-mode))

  (use-package evil-surround
    :commands (global-evil-surround-mode
	       evil-surround-edit
	       evil-Surround-edit
	       evil-surround-region)
    :config (global-evil-surround-mode 1))

  (use-package evil-textobj-anyblock
    :defer t
    :config
    (setq evil-textobj-anyblock-blocks
	  '(("(" . ")")
	    ("{" . "}")
	    ("\\[" . "\\]")
	    ("<" . ">"))))

  (use-package evil-easymotion
    :commands evilem-create evilem-default-keybindings
    :config
    ;; Use evil-search backend, instead of isearch
    (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
			:bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
			:bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
			:bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
			:bind ((evil-ex-search-highlight-all nil)))

    ;; Rebind scope of w/W/e/E/ge/gE evil-easymotion motions to the visible
    ;; buffer, rather than just the current line.
    (put 'visible 'bounds-of-thing-at-point (lambda () (cons (window-start) (window-end))))
    (evilem-make-motion evilem-motion-forward-word-begin #'evil-forward-word-begin :scope 'visible)
    (evilem-make-motion evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin :scope 'visible)
    (evilem-make-motion evilem-motion-forward-word-end #'evil-forward-word-end :scope 'visible)
    (evilem-make-motion evilem-motion-forward-WORD-end #'evil-forward-WORD-end :scope 'visible)
    (evilem-make-motion evilem-motion-backward-word-begin #'evil-backward-word-begin :scope 'visible)
    (evilem-make-motion evielem-motion-backward-WORD-begin #'evil-backward-WORD-begin :scope 'visible)
    (evilem-make-motion evilem-motion-backward-word-end #'evil-backward-word-end :scope 'visible)
    (evilem-make-motion evilem-motion-backward-WORD-end #'evil-backward-WORD-end :scope 'visible))

  (use-package evil-traces
    :after evil-ex
    :config
    (pushnew! evil-traces-argument-type-alist
	      '(+evil:align . evil-traces-global)
	      '(+evil:align-right . evil-traces-global))
    (evil-traces-mode))


  ;; Allows you to use the selection for * and #
  (use-package evil-visualstar
    :commands (evil-visualstar/begin-search
	       evil-visualstar/begin-search-forward
	       evil-visualstar/begin-search-backward)
    :init
    (evil-define-key* 'visual 'global
      "*" #'evil-visualstar/begin-search-forward
      "#" #'evil-visualstar/begin-search-backward))

  (use-package exato
    :commands evil-outer-xml-attr evil-inner-xml-attr)

(use-package evil-collection
  :after evil
  :straight t
  :config
  (evil-collection-init) )

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

(use-package org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

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
	 ("M-g f" . consult-flymake)               ; Alternative: consult-flycheck
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

(setq xterm-set-window-title t)
(setq visible-cursor nil)
;; Enable the mouse in terminal Emacs
(add-hook 'tty-setup-hook #'xterm-mouse-mode)
