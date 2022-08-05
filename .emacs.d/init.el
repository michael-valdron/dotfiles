;; Copyright 2022, Michael Valdron

;; Load scripts
(load-file (concat user-emacs-directory "package.el"))

;; Requires
(require 'use-package)

;; Set tab size
(setq-default tab-width 4)
 
;; Enable line numbers
(global-linum-mode t)

;; Enable autocomplete
(global-company-mode t)

;; Set editor theme
(use-package darcula-theme
	     :config (load-theme 'darcula t))

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
