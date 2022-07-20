;;; python_config --- Summary
;;; Commentary:
;;; code:

(defun enable-python-env (env)
  "ARGS: ENV."
  (interactive "senv: ")
  (pyvenv-mode 1)
  (pyvenv-activate (expand-file-name (concat "~/anaconda3/envs/"
                                             env "/")))
  (jedi:ac-setup))
;; (elpy-use-ipython) => is deprecated

(defun disable-python-env ()
  "Disable current python env."
  (interactive)
  (pyvenv-deactivate))


;;; python_config.el ends here
