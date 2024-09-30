;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; straight, vertico - marginalia - orderless - consult, company, projectile
;;; Code:

(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))
(require 'style)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	    (url-retrieve-synchronously
	     "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	     'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package straight
  :init
  (setq straight-use-package-by-default t))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


(use-package vertico
  :init
  (vertico-mode)
  :config (setq vertico-preselect 'first))

(use-package savehist
  :init
  (savehist-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
         ("C-x r b" . consult-bookmark)
         ("M-y" . consult-yank-pop)
         ("M-l" . consult-goto-line)
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-s r" . consult-ripgrep)
         ("M-s k" . consult-keep-lines)))

(use-package which-key
  :init (which-key-mode 1))

(use-package company
  :init (global-company-mode 1))

(use-package projectile
  :init
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/.emacs.d/" ("~/Developer/" . 2) ("/Users/akshitkr/Library/Mobile\ Documents/com~apple~CloudDocs/Documents/stuff/" . 4)))
  :bind (:map projectile-mode-map
              ("M-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package magit
  :bind ("C-x g" . magit))

(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode t)
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package bufler
  :config (bufler-mode)
  :bind ("C-x C-b" . bufler))

(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

(use-package spacious-padding
  :config
  (spacious-padding-mode 1))

(use-package yasnippet
  :init (yas-global-mode))
(use-package yasnippet-snippets)

;; Language setup
(use-package haskell-mode)
(use-package auctex)
(use-package pyvenv)
(use-package pdf-tools
  :config
  (pdf-tools-install))
(use-package apheleia
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff ruff-isort))
  (apheleia-global-mode +1))
(use-package flymake-ruff)
(use-package latex-preview-pane)
(use-package olivetti)
(provide 'init)
;;; init.el ends here
