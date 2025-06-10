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
(straight-use-package '(project :type built-in))
(use-package xref
  :straight t)
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
(use-package modus-themes
  :config
  (add-to-list 'default-frame-alist
             '(font . "-*-Source Code Pro-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1"))
  (load-theme 'modus-vivendi t))

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
  (setopt projectile-project-search-path '("~/.emacs.d/" ("~/Developer/" . 2) ("~/Documents/stuff/" . 4) ("~/Developer" . 2)))
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

(use-package racket-mode)
(use-package gptel
  :straight (gptel :type git :host github :repo "karthink/gptel" :branch "master")
  :config
  (gptel-make-anthropic "Anthropic"
                        :key (gptel-api-key-from-auth-source "api.anthropic.com")
                        :stream t)
  (setq gptel-include-reasoning t)
  (gptel-make-openai "TogetherAI"
    :host "api.together.xyz"
    :key (gptel-api-key-from-auth-source "api.together.xyz")
    :models '(meta-llama/Llama-4-Scout-17B-16E-Instruct deepseek-ai/DeepSeek-R1 Qwen/QwQ-32B)
    :stream t))



(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode)
  :config
  ;; Increase scale (1.5 = 150% of default size)
  (setq org-format-latex-options 
        (plist-put org-format-latex-options :scale 1.5))
  
  ;; Improve resolution (adjust dpi higher for sharper images)
  (setq org-preview-latex-default-process 'dvisvgm) ;; Use SVG for better quality
  (setq org-format-latex-options
        (plist-put org-format-latex-options :dpi 300)))
(use-package json-navigator)

(use-package eglot
  :straight t
  :ensure t
  :hook ((prog-mode . eglot-ensure))
  :config
  ;; Server configuration
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  
  ;; Performance settings
  (setq eglot-events-buffer-size 0)  ; Disable events buffer
  (setq eglot-sync-connect nil)      ; Connect asynchronously
  (setq eglot-events-buffer-size 0)       ; Disable events buffer completely
  (setq eglot-keep-workspace-alive nil)   ; Kill language server when last buffer closed
  
;; Reduce CPU usage
  (setq eglot-extend-to-xref nil)         ; Don't analyze non-project files
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider
                                          :documentFormattingProvider
                                          :documentRangeFormattingProvider
                                          :documentOnTypeFormattingProvider
                                          :colorProvider
                                          :foldingRangeProvider))

  ;; Improve responsiveness
  (setq read-process-output-max (* 1024 1024)) ; Increase read chunk size (1MB)
  (setq eglot-connect-timeout 10)         ; Fail fast if server doesn't respond

  ;; LSP session management
  (setq eglot-autoshutdown t)             ; Shutdown unused language servers
)

(provide 'init)
;;; init.el ends here
