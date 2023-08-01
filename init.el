;;; init.el --- Initialization file for Emacs
(setq warning-minimum-level :emergency)

;; basic cosmetic changes

;; startup message
(setq inhibit-startup-message t)

;; dont like it
(scroll-bar-mode -1)

;; eh
(tooltip-mode -1)

;; remove fringe
(fringe-mode 0)

;; i like the menu bar
(menu-bar-mode t)

;; nah
(tool-bar-mode -1)

;; recent files
(recentf-mode 1)

;; save file place 
(save-place-mode 1)
(global-auto-revert-mode 1)

;; font
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; delete selection when i type
(delete-selection-mode t)

;; track end of line
(setq track-eol t)
(global-auto-revert-mode t)
(setq truncate-lines t)
(global-hl-line-mode t)
(show-paren-mode t)

;; tab width setup
(setq-default tab-width 4)
(setq tab-width 4)

;; line numbers, native!
(global-display-line-numbers-mode)

;; better key mappings for mac
;; i dont use linux (yet) -- no point checking for OS
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq mac-control-modifier 'control)

(defvar native-comp-deferred-compilation-deny-list nil)
;; Install straight.el
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
(straight-use-package 'org)
(straight-use-package 'use-package)
(straight-use-package 'auctex)

(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))


(use-package exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; fonts
;; (set-frame-font "PragmataPro Mono Liga 14" nil t)
;; (set-frame-font "PragmataPro 12" nil t)
;; (set-frame-font "Inconsolata 12" nil t)
;; (set-frame-font "Overpass Mono 12")
(set-frame-font "Iosevka 12")
;; (set-frame-font "JuliaMono 12" nil t)

(use-package diminish
  :config
  (eval-after-load "company" '(diminish 'company-mode))
  (eval-after-load "company-box" '(diminish 'company-box-mode))
  (eval-after-load "undo-tree" '(diminish 'undo-tree-mode))
  (eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
  (eval-after-load "which-key" '(diminish 'which-key-mode))
  (diminish 'eldoc-mode)
)
;; fullscreen on macos
;; (toggle-frame-fullscreen)

;; vertico
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  :config
  (setq read-file-name-completion-ignore-case t
		read-buffer-completion-ignore-case t
		completion-ignore-case t))

(use-package savehist
  :init
  (savehist-mode))

;; marginalia
(use-package marginalia
  :init
  (marginalia-mode +1))

;; consult
(use-package consult
  :bind
  ("C-x b" . 'consult-buffer)
  ("C-s" . 'consult-line)
  ("C-r" . 'consult-line))


;; install flycheck
(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers
				'(emacs-lisp-checkdoc))
  )

;; better scrolling
(setq scroll-conservatively 101)

;; company mode -- moved to corfu -- moved back to company
(use-package company
  :ensure company-box
  :ensure company-php 
  :ensure company-web 
  :init
  (global-company-mode t)
  (global-set-key (kbd "M-/") 'company-complete)
  ;; Complete quite soon
  :custom
  (company-minimum-prefix-length 3)
  (company-idle-delay 0.1)
  (company-show-quick-access "off")  
  (company-quick-access-hint-function (lambda (param) " unknown"))
)

;;electric-pair
(electric-pair-mode t)

;; tab width

(add-hook 'python-mode-hook
      (lambda ()
        (setq tab-width 4)
        (setq python-indent-offset 4)
)
)

;; better c defaults
(setq c-default-style "linux"
      c-basic-offset 4)

;; theme
(setq modus-themes-mode-line
	  '(accented borderless padded))
(setq modus-themes-region
	  '(bg-only))
(setq modus-themes-bold-constructs t)
(setq modus-themes-italic-constructs t)
(setq modus-themes-paren-match
	  '(bold intense))
(load-theme 'modus-vivendi)

;; projectile
(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map
			  (kbd "s-p")
			  'projectile-command-map)
  (define-key projectile-mode-map
			  (kbd "C-c p")
			  'projectile-command-map)
  (setq projectile-project-search-path
		'(("~/Developer" . 3)
		  ("~/Documents/" . 5)
		  )))

;; neotree
(use-package neotree)

;; disable ring bell
(setq ring-bell-function 'ignore)

;; vterm
(use-package vterm)

;; magit
(use-package magit)

;; haskell setup
(use-package haskell-mode)

;; rainbow
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; /way/ better scrolling
(pixel-scroll-precision-mode t) ;; emacs 29 only!

(defalias 'yes-or-no-p 'y-or-n-p)
(use-package eros
  :config
  (eros-mode 1))

(use-package yasnippet-snippets)
(use-package yasnippet
  :config
  (yas-global-mode 1))

(add-hook 'org-mode-hook 'visual-line-mode)

;; roam setup
(setq org-default-notes-file "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/notes.org")

;; code formatting
(use-package apheleia
  :config
  (add-to-list 'apheleia-mode-alist '(typescriptreact-mode . prettier-typescript))
  (apheleia-global-mode +1)
  )

(use-package avy
  :config
  (global-set-key (kbd "C-z") 'avy-goto-char-2))

;; irc stuff
(setq url-proxy-services
      '(("http"     . "proxy2.iiit.ac.in:80")))

(setq erc-server "irc.libera.chat"
      erc-nick "komikat"    ; 
      erc-user-full-name "Akshit Kumar"  ; And this!
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("Libera.Chat" "#haskell" "#emacs"))
      erc-kill-buffer-on-part t
            erc-auto-query 'bury)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)        
   ("C-;" . embark-dwim)       
   ("C-h B" . embark-bindings)))

(use-package undo-tree
  :config
  (global-undo-tree-mode))


(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "ipython3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(use-package ein)

(global-set-key [f1] 'kill-emacs)

(defun show-in-finder ()
  (interactive)
  (shell-command (concat "open -R " buffer-file-name)))


(use-package ob-ipython)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ipython . t)
   ))

(setq python-indent-offset 4)
(setq python-indent-guess-indent-offset t)
(setq python-indent-guess-indent-offset-verbose nil)


(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; display init time
(defun display-startup-echo-area-message
	()
  (message
   (format "Emacs took %s seconds to boot up."
		   (emacs-init-time))))

