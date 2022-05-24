;; -*- lexical-binding: t; -*-

;; Since Emacs 27, an early configuration file early-init.el can be provided to
;; handle initialization to be done before init.el is loaded.

(setq-default load-prefer-newer t           ; to reduce the risk of loading outdated byte code files
              package-enable-at-startup nil ; package initialization occurs after `early-init-file'.
              package-native-compile t)     ; native is better, I guess?

(tooltip-mode -1)    ; disable tooltips
(menu-bar-mode -1)   ; disable the menu bar

(if (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))         ; disable the toolbar
(if (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))       ; disable visible scrollbar

(add-to-list 'default-frame-alist '(undecorated . t))
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
