;; -*- lexical-binding: t; -*-


;; Since Emacs 27, an early configuration file early-init.el can be provided to
;; handle initialization to be done before init.el is loaded.

;;; Code:

(setq-default load-prefer-newer t
              package-enable-at-startup nil
              package-native-compile t)

(setopt gc-cons-threshold most-positive-fixnum
	      gc-cons-percentage 1.0)

(defun bore/gc-after-focus-change ()
  "Run GC when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))

(defun bore/reset-init-values ()
  "Restore default values after init."
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setopt gc-cons-threshold (* 20 1024 1024)
	           gc-cons-percentage 0.1)
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function #'bore/gc-after-focus-change)))))

(add-hook 'emacs-startup-hook 'bore/reset-init-values)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name "var/eln-cache/" user-emacs-directory))))

;; Set the right directory to store the native comp cache
;; (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(setq-default load-prefer-newer noninteractive)

(setopt package-enable-at-startup nil
	      native-comp-async-report-warnings-errors nil
	      server-client-instructions nil
	      frame-inhibit-implied-resize t
	      fancy-startup-text nil
	      fancy-about-text nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; (add-to-list 'default-frame-alist '(undecorated . t))

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;;; early-init.el ends here
