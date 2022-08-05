;; Copyright 2022, Michael Valdron

;; Requires
(require 'package)

;; Define and refresh package repos
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(defun install-packages (&rest packages)
  "Installs packages to use with emacs."
  (dolist (pkg packages)
    ;; Install package if not installed
    (unless (package-installed-p pkg)
      (package-refresh-contents)
      (package-install pkg))))

;; Init install packages
(install-packages
 'use-package
 'real-auto-save
 'company
 'go-mode)

;; Variables
(setq use-package-always-ensure 't)
