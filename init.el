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
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package auctex
  :defer t)

(use-package ctrlf
  :init
  (ctrlf-mode t))

;;; VERTICO BEGIN
(use-package vertico
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

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
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  :config
  ;; THEME BEGIN
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-mixed-fonts t)

  (set-frame-font "Iosevka 12")
  (add-to-list 'default-frame-alist
             '(font . "Iosevka 12"))


  (load-theme 'modus-vivendi)
  (define-key global-map (kbd "<f5>") #'modus-themes-toggle)
  ;; THEME END

  ;; basic cosmetic changes
  (pixel-scroll-precision-mode t)       ; emacs 29 - in the future now
  (setq ring-bell-function 'ignore)     ; so annoying
  (setq inhibit-startup-message t)      ; ^^
  (scroll-bar-mode -1)                  ; the scroll thing on the right
  (tooltip-mode -1)                     ; eh
  (fringe-mode 0)                       ; the fringe
  (menu-bar-mode t)                     ; love it - the top thing on mac
  (tool-bar-mode -1)                    ; nah
  (recentf-mode 1)                      ; recent files
  (delete-selection-mode t)             ; delete selection when typing
  (global-hl-line-mode -1)              ; on the fence
  (show-paren-mode t)                   ; NEED
  (global-display-line-numbers-mode t)  ; do i REALLY need line numbers? not sure.
  (setq display-line-numbers-type 'relative)

  ;; tab width stuff
  (setq-default indent-tabs-mode nil)     ; use spaces!
  (setq-default tab-width 4)              ; 4 spaces for 1 tab
  (setq tab-width 4)

  ;; mac keybinds
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta
        mac-right-option-modifier 'none)
  
  :hook (emacs-startup . (lambda ()
                           (custom-set-faces
                            '(line-number ((t (:inherit default :font "Iosevka")))))))
)


 

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

;;; TREE SITTER BEGIN
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	 (haskell "https://github.com/tree-sitter/tree-sitter-haskell")
	 (c "https://github.com/tree-sitter/tree-sitter-c")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)
   (c-mode . c-ts-mode)))
;;; TREE SITTER END

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i --simple-prompt")

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
              ("C-c C-f" . eglot-format-buffer))
  :hook ((python-mode . eglot-ensure)
         (python-mode . flyspell-prog-mode)
         (python-mode . superword-mode)
         (python-mode . hs-minor-mode)
         (python-mode . (lambda () (set-fill-column 88))))
  :config
  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"]
                             :plugins (
                                       :pycodestyle (:enabled :json-false)
                                       :mccabe (:enabled :json-false)
                                       :pyflakes (:enabled :json-false)
                                       :flake8 (:enabled :json-false
                                                :maxLineLength 88)
                                       :ruff (:enabled t
                                              :lineLength 88)
                                       :pydocstyle (:enabled t
                                                    :convention "numpy")
                                       :yapf (:enabled :json-false)
                                       :autopep8 (:enabled :json-false)
                                       :black (:enabled t
                                               :line_length 88
                                               :cache_config t)))))))

(use-package racket-mode
  :hook (racket-mode . racket-xp-mode))

(use-package paredit
  :ensure t
  :config
  (dolist (m '(emacs-lisp-mode-hook
               racket-mode-hook
               racket-repl-mode-hook))
    (add-hook m #'paredit-mode))
  (bind-keys :map paredit-mode-map
             ("{"   . paredit-open-curly)
             ("}"   . paredit-close-curly))
  (unless terminal-frame
    (bind-keys :map paredit-mode-map
               ("M-[" . paredit-wrap-square)
               ("M-{" . paredit-wrap-curly))))
(use-package eros
  :config
  (eros-mode 1))

(use-package projectile
  :init
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/.emacs.d/" ("~/Developer/" . 2)))
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
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package bufler
  :init (bufler-mode)
  :bind ("C-x C-b" . bufler)
)

;; elfeed config
(use-package elfeed
  :bind ("C-x w" . elfeed)
  :init ;; Somewhere in your .emacs file
  (setq elfeed-feeds
      '(("http://nullprogram.com/feed/" blog emacs)
        ("https://www.reddit.com/user/nxlyd/m/compsci.rss")
        ("https://hnrss.org/frontpage"))
      )
  )

(use-package devdocs
  :bind ("C-h D" . devdocs-lookup))

(use-package ein)

(use-package gptel
  :config
  (setq gptel-api-key (getenv "OPENAPI")))

(use-package solaire-mode
  :hook (after-init . solaire-global--mode))

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
  (setq erc-prompt-for-nickserv-password nil)
  (setq erc-nickserv-passwords
        `((freenode     (("komikat" . ,freenode-nickone-pass)))))
  (setq erc-nick "komikat"
        erc-user-full-name "Akshit Kumar")
  )

(defun connect-znc ()
  (interactive)
  (erc :server "localhost"
       :port   "1025"
       :user "akshitkr"
       :password znc-pass)) 

;; init.el ends here
