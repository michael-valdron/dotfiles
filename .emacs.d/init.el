;; Copyright 2022, Michael Valdron

;; Define and refresh package repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install deps
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Requires
(require 'use-package)

;; Variables
(setq use-package-always-ensure 't)

;; Enable line numbers
(global-linum-mode t)

;; Enable autocomplete
;; (global-autocomplete t)

(use-package darcula-theme
	     :config (load-theme 'darcula t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(exotica-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
