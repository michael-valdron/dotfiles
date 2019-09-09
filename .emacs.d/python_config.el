;;; python_config --- Summary
;;; Commentary:
;;; code:

(defun enable-python-env (env)
  "ARGS: ENV."
  (interactive "senv: ")
  (setenv "WORKON_HOME" (expand-file-name (concat "~/.conda/envs/" env "/")))
  (pyvenv-mode 1))
;; (elpy-use-ipython) => is deprecated

(defun disable-python-env ()
  "Disable current python env."
  (interactive)
  (pyvenv-deactivate))


;;; python_config.el ends here
