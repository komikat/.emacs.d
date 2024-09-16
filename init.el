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
  :init
  (setq straight-use-package-by-default t))

(use-package vertico
  :init
  (vertico-mode)
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
        mac-right-option-modifier 'none))

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
(add-to-list 'default-frame-alist
             '(font . "-*-SF Mono-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1"))


(provide 'init)
;;; init.el ends here
