;;; init.el --- Initialization file for Emacs

;; basic cosmetic changes
(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tooltip-mode -1)
(fringe-mode 0)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; line numbers, native!
(global-display-line-numbers-mode)

;; better key mappings for mac
;; i dont use linux (yet) -- no point checking for OS
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-control-modifier 'control)

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom (straight-use-package-by-default t))

;; font
(set-frame-font "Iosevka 18" nil t)

;; fullscreen on macos
;;(toggle-frame-fullscreen)

;; selectrum
(use-package selectrum
  :config
  (selectrum-mode +1))

;; marginalia
(use-package marginalia
  :after (selectrum)
  :init (marginalia-mode +1))

;; consult
(use-package consult
  :bind
  ("C-x b" . 'consult-buffer))

(use-package selectrum-prescient
  :after (selectrum)
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Emacs 28: Hide commands in M-x which do not apply to the current mode.
  ;; Corfu commands are hidden, since they are not supposed to be used via M-x.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; install flycheck
(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
)

;; better scrolling
(setq scroll-conservatively 101)

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))




(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-headerline-breadcrumb-enable nil
	lsp-modeline-diagnostics-enable nil
	lsp-diagnostics-provider :none
	)
  ;;(setq lsp-disabled-clients '(clangd))
  
  :hook ((python-mode . lsp)
		 (c-mode . lsp)
		 (c++-mode . lsp)
		 (lsp-mode . lsp-enable-which-key-integration))
  :commands
         lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-idle-delay 0.500)
(setq lsp-use-plists t)

;; company mode
(use-package company
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.1)
)

;;electric-pair
(electric-pair-mode t)

;; tab width
(setq-default tab-width 4)

;; better c defaults
(setq c-default-style "linux"
          c-basic-offset 4)

;; theme
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))

(load-theme 'modus-vivendi)


;; projectile
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-search-path '(("~/Developer" . 3) ("~/" . 1))))


;; neotree
(use-package neotree
  :ensure t)

;; disable ring bell
(setq ring-bell-function 'ignore)

;; sidekick
(straight-use-package
 '(sidekick :type git :host github :repo "VernonGrant/sidekick.el"))
(require 'sidekick)

;; Set some default bindings.
(global-set-key (kbd "C-c k") 'sidekick-at-point)
(global-set-key (kbd "C-c K") 'sidekick-focus-toggle)
(global-set-key (kbd "C-c C-k") 'sidekick-search-for-literal)

;; vterm
(use-package vterm
  :ensure t)

;; magit
(use-package magit
  :ensure t)

(use-package yaml-mode
  :ensure t)
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(add-hook 'python-mode-hook #'flycheck-python-setup)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

