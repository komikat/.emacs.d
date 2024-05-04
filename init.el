;;; init.el --- Initialization file for Emacs
;;; Commentary: very opinionated, opinions commented with most decisions
;;; Code:

;; straight.el
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

;; install use-package
(straight-use-package 'use-package)


;; custom file setup -- needed for some reason
(defvar custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))



(use-package auctex
  :defer t)

(use-package ctrlf
  :init
  (ctrlf-mode t))

;;; VERTICO BEGIN
(use-package vertico
  :init
  (vertico-mode)
  :config (defvar vertico-preselect 'first))

(use-package savehist
  :init
  (savehist-mode))

(use-package modus-themes
  :init
  (defvar modus-themes-italic-constructs t)
  (defvar modus-themes-bold-constructs nil)
  (defvar modus-themes-region '(bg-only no-extend))
  (defvar modus-themes-common-palette-overrides
        '((fg-prompt cyan)
          (bg-prompt bg-cyan-nuanced)))
  :config
  (defvar modus-themes-to-toggle '(modus-vivendi modus-operandi-tritanopia))
  (defvar modus-themes-prompts '(regular))
  (set-face-attribute 'default nil
                      :font "Hasklig")
  (load-theme 'modus-vivendi)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))
  
;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (defvar minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (defvar read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (defvar enable-recursive-minibuffers t)
  :config
  ;; THEME BEGIN

  ;; THEME END

  ;; basic cosmetic changes
  (pixel-scroll-precision-mode t)     ; emacs 29 - in the future now
  (defvar ring-bell-function 'ignore)   ; so annoying
  (defvar inhibit-startup-message t)    ; ^^
  (scroll-bar-mode -1)                ; the scroll thing on the right
  (tooltip-mode -1)                   ; eh
  (fringe-mode 0)                     ; the fringe
  (menu-bar-mode t)                   ; love it - the top thing on mac
  (tool-bar-mode -1)                  ; nah
  (recentf-mode 1)                    ; recent files
  (delete-selection-mode t)           ; delete selection when typing
  (global-hl-line-mode -1)            ; on the fence
  (show-paren-mode t)                 ; NEED
  (global-display-line-numbers-mode t) ; do i REALLY need line numbers? not sure.
  (defvar display-line-numbers-type 'relative)

  ;; tab width stuff
  (setq-default indent-tabs-mode nil)   ; use spaces!
  (setq-default tab-width 4)            ; 4 spaces for 1 tab
  (defvar tab-width 4)

  ;; mac keybinds
  (defvar mac-option-modifier 'super)
  (defvar mac-command-modifier 'meta)
  (defvar mac-right-option-modifier 'none)
  :hook (emacs-startup . (lambda ()
                           (custom-set-faces
                            '(line-number ((t (:inherit default :font "Hasklig"))))))))
  


 

(use-package orderless
  :init
  (defvar completion-styles '(orderless basic))
  (defvar completion-category-defaults nil)
  (defvar completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))
;;; VERTICO END

;;; CONSULT BEGIN 
(use-package consult
  :bind (("C-x b" . consult-buffer)              
         ("C-x r b" . consult-bookmark)          
         ("M-y" . consult-yank-pop)              
         ("M-l" . consult-goto-line)             
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-s r" . consult-ripgrep)
         ("M-s k" . consult-keep-lines)))
;;; CONSULT END



(defvar python-shell-interpreter "ipython")
(defvar python-shell-interpreter-args "-i --simple-prompt")

(use-package which-key
  :init (which-key-mode 1))

(use-package company
  :init (global-company-mode 1))

(use-package haskell-mode)
(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
              ("C-c C-d" . eldoc)
              ("C-c C-e" . eglot-rename)
              ("C-c C-o" . python-sort-imports)
              ("C-c C-f" . eglot-format-buffer)))

(use-package racket-mode
  :hook (racket-mode . racket-xp-mode)
  :config
  (defvar racket-eldoc-function 'racket-xp-eldoc-function))

;; moved to parinfer

;; (use-package paredit
;;   :ensure t
;;   :config
;;   (dolist (m '(emacs-lisp-mode-hook
;;                racket-mode-hook
;;                racket-repl-mode-hook))
;;     (add-hook m #'paredit-mode))
;;   (bind-keys :map paredit-mode-map
;;              ("{"   . paredit-open-curly)
;;              ("}"   . paredit-close-curly))
;;   (unless terminal-frame
;;     (bind-keys :map paredit-mode-map
;;                ("M-[" . paredit-wrap-square)
;;                ("M-{" . paredit-wrap-curly))))

(use-package eros
  :config
  (eros-mode 1))

(use-package projectile
  :init
  (projectile-mode +1)
  (defvar projectile-project-search-path '("~/.emacs.d/"
                                           ("~/Developer/" . 2)
                                           ("~/Documents/stuff" . 6)
                                           ("/Users/akshitkr/Library/Mobile Documents/com~apple~CloudDocs/Documents/College/" . 3)))
  :bind (:map projectile-mode-map
              ("M-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package magit
  :bind ("C-x g" . magit))

(use-package pdf-tools
  :config
  (pdf-tools-install))

(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(use-package latex-preview-pane)

; (use-package org-noter)

(use-package rainbow-delimiters
  :config
  (rainbow-delimiters-mode t)
  :hook (prog-mode . rainbow-delimiters-mode))
  

(use-package bufler
  :init (bufler-mode)
  :bind ("C-x C-b" . bufler))


;; elfeed config
(use-package elfeed
  :bind ("C-x w" . elfeed)
  :init ;; Somewhere in your .emacs file
  (defvar elfeed-feeds
      '(("http://nullprogram.com/feed/" blog emacs)
        ("https://www.reddit.com/user/nxlyd/m/compsci.rss")
        ("https://hnrss.org/frontpage"))))
      
  

(use-package devdocs
  :bind ("C-h D" . devdocs-lookup))

(use-package gptel
  :config
  (defvar gptel-api-key (getenv "OPENAPI")))

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

(use-package spacious-padding
  :config
  (spacious-padding-mode 1))

(use-package haskell-mode)

(load "~/.emacs.d/.ercpass")
(use-package erc
  :config
  (defvar erc-prompt-for-nickserv-password nil)
  (defvar erc-nickserv-passwords
        `((freenode     (("komikat" . ,freenode-nickone-pass)))))
  (defvar erc-nick "komikat")
  (defvar erc-user-full-name "Akshit Kumar"))
  

(defun connect-znc ()
  (interactive)
  (erc :server "localhost"
       :port   "1200"
       :user "akshitkr"
       :password znc-pass))

(defun connect-without-znc ()
  (interactive)
  (erc :server "irc.libera.chat"
       :port "6697"))

;; (use-package ligature
;;   :config
;;   ;; Enable all Iosevka ligatures in programming modes
;;   (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
;;                                        "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
;;                                        "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
;;                                        ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
;;   ;; Enables ligature checks globally in all buffers. You can also do it
;;   ;; per mode with `ligature-mode'.
;;   (global-ligature-mode t))

(use-package hasklig-mode
  :hook (haskell-mode))

(use-package lsp-mode
  :init
  (defvar lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-haskell
  :config
  (defvar lsp-haskell-server-path "/Users/akshitkr/.ghcup/bin/haskell-language-server-wrapper"))

(use-package lsp-ui)
(use-package moody)

;; https://www.lucacambiaghi.com/vanilla-emacs/readme.html#h:7BA73F60-D31F-4A96-9DEE-02A4FC1BEE8B
;; zmq installation:

;; Need to have automake, autoconf
;; In straight/build/zmq/src run autoreconf -i
;; In straight/build/zmq run make
;; emacs-zmq installation:

;; In straight/build/emacs-zmq run wget https://github.com/nnicandro/emacs-zmq/releases/download/v0.10.10/emacs-zmq-x86_64-apple-darwin17.4.0.tar.gz
;; Then tar -xzf emacs-zmq-x86_64-apple-darwin17.4.0.tar.gz
;; Finally cp emacs-zmq-x86_64-apple-darwin17.4.0/emacs-zmq.so emacs-zmq.dylib
(use-package zmq)
(use-package jupyter)
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-vale
  :init (flycheck-vale-setup))

(use-package yasnippet
  :init (yas-global-mode))

(use-package yasnippet-snippets)
(use-package ruff-format)
(add-hook 'python-mode-hook 'ruff-format-on-save-mode)
(use-package reformatter)

(setq-default flycheck-disabled-checkers '(python-mypy))

(defvar company-dabbrev-downcase 0)
(defvar company-idle-delay 0)

(use-package avy
  :config
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume)
  (global-set-key (kbd "C-;") 'avy-goto-char-timer))

(defun my-turn-off-line-numbers ()
  "Disable line numbering in the current buffer."
  (display-line-numbers-mode -1))

(add-hook 'pdf-view-mode-hook #'my-turn-off-line-numbers)



(use-package parinfer-rust-mode
    :hook emacs-lisp-mode
    :init
    (defvar parinfer-rust-auto-download t))


(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))

(use-package treesit-auto
  :demand t
  :config
  (global-treesit-auto-mode))

(use-package keyfreq)
(use-package command-log-mode)

(use-package elixir-mode
  :ensure t)
;; init.el ends here
