;;; utils.el --- useful minor modes

(use-package yasnippet
  :init (yas-global-mode))
(use-package yasnippet-snippets)
(use-package ruff-format
  :hook (python-mode . ruff-format-on-save-mode))
(use-package reformatter)
(use-package apheleia
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff ruff-isort))
  (apheleia-global-mode +1))
(use-package flymake-ruff)
(use-package latex-preview-pane)
(use-package pyvenv)
(provide 'utils)
