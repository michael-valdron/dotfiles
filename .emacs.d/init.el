;; Copyright 2022, Michael Valdron

;; Load scripts
(load-file (concat user-emacs-directory "package.el"))

;; Requires
(require 'use-package)

;; Set tab size
(setq-default tab-width 4)

;; Set temporary file directory, referenced from: https://www.emacswiki.org/emacs/AutoSave
(let ((tmp-dir "~/.tmp"))
  (unless (file-directory-p tmp-dir)
    (make-directory tmp-dir))
  (setq temporary-file-directory tmp-dir))
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Enable line numbers
(global-linum-mode t)

;; Enable autocomplete
(global-company-mode t)

;; Set editor theme
(use-package monokai-theme
	     :config (load-theme 'monokai t))

;; Set Transparent Background, referenced from: https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
(defun on-after-init ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))
(add-hook 'window-setup-hook 'on-after-init)

;; Custom Variables
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(go-mode use-package exotica-theme darcula-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
