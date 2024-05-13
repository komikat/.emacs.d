;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; very opinionated, obviously
;;; Code:

;; straight.el steup
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
  :custom
  (straight-use-package-by-default t))

;; general packages setup
(use-package ctrlf
  :commands (ctrlf-mode)
  :init
  (ctrlf-mode t))

(use-package vertico
  :commands (vertico-mode)
  :init
  (vertico-mode)
  (defvar vertico-preselect)
  :config (setq vertico-preselect 'first))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :init
  (setq enable-recursive-minibuffers t)
  
  :config
  (electric-pair-mode t)
  (pixel-scroll-precision-mode t)     ; emacs 29 - in the future now
  (setq ring-bell-function 'ignore)   ; so annoying
  (setq inhibit-startup-message t)    ; ^^
  (scroll-bar-mode -1)                ; the scroll thing on the right
  (tooltip-mode -1)                   ; eh
  (fringe-mode 1)                     ; the fringe
  (menu-bar-mode t)                   ; love it - the top thing on mac
  (tool-bar-mode -1)                  ; nah
  (recentf-mode 1)                    ; recent files
  (delete-selection-mode t)           ; delete selection when typing
  (global-hl-line-mode -1)            ; on the fence
  (show-paren-mode t)                 ; NEED
  (global-display-line-numbers-mode t)


  ;; tab width stuff
  (setq-default indent-tabs-mode nil) ; use spaces for intendation -- not hard tabs
  (setq-default tab-width 4)            ; 4 spaces for 1 tab

  ;; mac keybinds
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta
        mac-right-option-modifier 'none)
  
  :hook (emacs-startup . (lambda ()
                           (custom-set-faces
                            '(line-number ((t (:inherit default :font "Hasklig"))))))))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :commands (marginalia-mode)
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
  :commands (which-key-mode)
  :init (which-key-mode 1))

(use-package company
  :commands (global-company-mode)
  :init (global-company-mode 1))

(use-package projectile
  :commands (projectile-mode)
  :init
  (defvar projectile-project-search-path)
  (defvar projectile-mode-map)
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/.emacs.d/" ("~/Developer/" . 2) ("/Users/akshitkr/Library/Mobile Documents/com~apple~CloudDocs/Documents/College/" . 3)))
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
  :commands (bufler-mode)
  :config (bufler-mode)
  :bind ("C-x C-b" . bufler))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package golden-ratio
  :commands (golden-ratio-mode)
  :config
  (golden-ratio-mode 1))

(use-package spacious-padding
  :commands (spacious-padding-mode)
  :config
  (spacious-padding-mode 1))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package yasnippet
  :commands (yas-global-mode)
  :init (yas-global-mode))

(use-package yasnippet-snippets)

(use-package eglot)

(use-package flycheck-eglot
  :commands (global-flycheck-eglot-mode)
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

;; theme
(use-package doom-themes
  :commands (doom-themes-org-config)
  :init
  (defvar doom-themes-enable-bold nil)
  (defvar doom-themes-enable-italic nil)
  :config
  ;; i like meltbus and homage
  (load-theme 'doom-homage-black t)
  (doom-themes-org-config))

;; Language setup
(use-package haskell-mode)
(use-package auctex)

(provide 'init)
;;; init.el ends here
