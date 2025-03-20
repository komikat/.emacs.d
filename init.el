;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; straight, vertico - marginalia - orderless - consult, company, projectile
;;; Code:

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

;;; STYLES
(setq mac-option-modifier 'super
      mac-command-modifier 'meta
      mac-right-option-modifier 'none
      ring-bell-function 'ignore
      inhibit-startup-message t)
(setq-default indent-tabs-mode nil
              tab-width 4)
(dolist (mode '(electric-pair-mode
                pixel-scroll-precision-mode
                menu-bar-mode
                recentf-mode
                delete-selection-mode
                global-display-line-numbers-mode
                fringe-mode
                show-paren-mode))
  (funcall mode 1))
(dolist (mode '(scroll-bar-mode
                tooltip-mode
                tool-bar-mode
                global-hl-line-mode))
  (funcall mode -1))
(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))
(add-to-list 'default-frame-alist
             '(font . "-*-Inconsolata-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1"))
(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode t)
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package golden-ratio
  :config
  (golden-ratio-mode 1))
(use-package spacious-padding
  :config
  (spacious-padding-mode 1))

;;; COMPLETION
(use-package vertico
  :init (vertico-mode)
  :config (setopt vertico-preselect 'first))
(use-package savehist
  :init
  (savehist-mode))
(use-package orderless
  :config
  (setopt completion-styles '(orderless basic))
  (setopt completion-category-defaults nil)
  (setopt completion-category-overrides '((file (styles partial-completion)))))
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
(use-package avy
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume)
  (global-set-key (kbd "C-;") 'avy-goto-char-timer))

;;; PROJECTS
(use-package projectile
  :init
  (setopt projectile-project-search-path '("~/.emacs.d/" ("~/Developer/" . 2) ("~/Documents/stuff/" . 4)))
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("M-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (with-eval-after-load 'magit-mode
    (add-hook 'after-save-hook 'magit-after-save-refresh-status t)))

;;; UTILS
(use-package yasnippet
  :init (yas-global-mode))
(use-package ruff-format
  :hook (python-mode . ruff-format-on-save-mode))
(use-package reformatter)
(use-package apheleia
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff ruff-isort)))
(use-package flymake-ruff)
(use-package latex-preview-pane)
(use-package pyvenv)
(use-package paredit)
(require 'auth-source)

;;; MAJOR MODES
; (use-package haskell-mode)
; (use-package auctex)
(use-package markdown-mode)
(use-package pdf-tools
  :config
  (pdf-tools-install)
  :hook (pdf-view-mode . (lambda () "turn off line numbers" (display-line-numbers-mode -1))))
(use-package erc
  :config
  (setopt erc-modules
          (seq-union '(sasl scrolltobottom services)
                     erc-modules))
  (setopt erc-use-auth-source-for-nickserv-password t)
  (setopt erc-nickserv-passwords
          `((libera (("komikat" . ,(auth-source-pick-first-password
                                    :host "irc.libera.chat"
                                    :user "komikat"))))))
  (setopt erc-sasl-user "komikat")
  :custom
  (erc-sasl-use-sasl t))
(use-package racket-mode)
(use-package gptel
  :straight (:host github :repo "karthink/gptel" :tag "v0.9.8")
  :config
  ;; Common key retrieval function
  (defun get-api-key (host)
    "Retrieve API key from .authinfo for specified HOST."
    (let ((auth-info (auth-source-search :host host
                                         :require '(:secret))))
      (when auth-info
        (let ((secret (plist-get (car auth-info) :secret)))
          (if (functionp secret)
              (funcall secret)
            secret)))))
  ;; New Anthropic configuration
  (gptel-make-anthropic "Anthropic"
    :key (get-api-key "api.anthropic.com")
    :stream t
    :models '(claude-3-opus-latest
              claude-3-5-haiku-latest
              claude-3-7-sonnet-latest
              claude-3-5-sonnet-latest))
  (setq gptel-model 'claude-3-5-haiku-latest)
  (setq gptel-include-reasoning t))

(provide 'init)
;;; init.el ends here
