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
(let
	((bootstrap-file
	  (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	 (bootstrap-version 5))
  (unless
	  (file-exists-p bootstrap-file)
	(with-current-buffer
		(url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
	  (goto-char
	   (point-max))
	  (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package
(straight-use-package 'org)
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))

;; font
;; (set-frame-font "PragmataPro Mono Liga 12" nil t)
;; (set-frame-font "PragmataPro 13" nil t)
(set-frame-font "Iosevka 12" nil t)

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Helvetica Neue" :height 180 :weight Light)))))


(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;; fullscreen on macos
;; (toggle-frame-fullscreen)

;; selectrum -- depracated
;; vertico
(use-package vertico
  :straight (:files (:defaults "extensions/*"))
  :init
  (vertico-mode)
  (vertico-buffer-mode)
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
)



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

;; company mode -- moved to corfu
(use-package corfu
  :hook (lsp-completion-mode . kb/corfu-setup-lsp)
  :straight (:files (:defaults "extensions/*"))
  :bind
  (("<escape>" . corfu-quit)
   ("SPC" . corfu-insert-separator))
  :custom
  (corfu-cycle t)		;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)		;; Enable auto completion
  (corfu-separator ?\s)	;; Orderless field separator
  (corfu-quit-at-boundary nil) ;; Never quit at completion boundary
  (corfu-quit-no-match nil)	;; Never quit, even if there is no match
  (corfu-preselect-first t)      ;; Preselect the prompt
  (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  (corfu-scroll-margin 0) ;; Use scroll margin
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)       ; Always have the same width
  (corfu-count 10)
  (corfu-echo-documentation t)
  (completion-cycle-threshold nil)
  (corfu-quit-at-boundary 'separator)     ; a non-nil value is necessary
  (corfu-separator ?\s)                   ; Use space
  (corfu-quit-no-match 'separator) ; Don't quit if there is `corfu-separator' inserted
  (corfu-preview-current 'insert)  ; Preview current candidate?
  (corfu-popupinfo-mode t)
  (lsp-completion-provider :none)

  :config
  (defun kb/corfu-setup-lsp ()
	"Use orderless completion style with lsp-capf instead of the
  default lsp-passthrough."
	(setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :init
  (global-corfu-mode)
)

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-ispell)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)


(use-package lsp-mode
  :after corfu
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-disabled-clients '(jsts-ls deno-ls))
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp
  )


(use-package lsp-ui :commands lsp-ui-mode)
;; optional if you want which-key integration


(use-package which-key
  :config
  (which-key-mode))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max
	  (* 1024 1024))

;; 1mb
(setq lsp-idle-delay 0.500)
(setq lsp-use-plists t)

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))



(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'
)

;;electric-pair
(electric-pair-mode t)

;; tab width
(setq-default tab-width 4)
(setq tab-width 4)

(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode t)
        (setq tab-width 4)
        (setq python-indent-offset 4)))

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
		  ("~/" . 1))))

;; neotree
(use-package neotree)

;; disable ring bell
(setq ring-bell-function 'ignore)

;; sidekick
(straight-use-package
 '(sidekick :type git :host github :repo "VernonGrant/sidekick.el"))
(require 'sidekick)

;; Set some default bindings.
(global-set-key
 (kbd "C-c k")
 'sidekick-at-point)
(global-set-key
 (kbd "C-c K")
 'sidekick-focus-toggle)
(global-set-key
 (kbd "C-c C-k")
 'sidekick-search-for-literal)

;; vterm
(use-package vterm)

;; magit
(use-package magit)

;; haskell setup
(use-package haskell-mode)

;; display init time
(defun display-startup-echo-area-message
	()
  (message
   (format "Emacs took %s seconds to boot up."
		   (emacs-init-time))))

;; racket mode
(use-package racket-mode
  :init
  (setq racket-program "racket"))
(use-package paredit
  :init
  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-)
  )

;; rainbow
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; /way/ better scrolling
(pixel-scroll-precision-mode t) ;; emacs 29 only!

(use-package go-mode)
(use-package tex
  :straight auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(defalias 'yes-or-no-p 'y-or-n-p)
(use-package yaml-mode)
(use-package eros
  :config
  (eros-mode 1))
(use-package ox-gfm
  :init
  (eval-after-load "org"
	'(require 'ox-gfm nil t)))

(use-package yasnippet-snippets)
(use-package yasnippet
  :config
  (yas-global-mode 1))

;; org mode shinings
(setq org-hide-emphasis-markers t)
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(add-hook 'org-mode-hook 'visual-line-mode)

;; roam setup
(setq org-default-notes-file "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/notes.org")
(use-package org-roam)
(setq org-roam-directory "~/Documents/org/zettel")
(org-roam-db-autosync-mode)

(use-package org-roam-ui)
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

;; javascsript stuff
(use-package js2-mode)
(use-package rjsx-mode)

(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2))
  
(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
;  (add-to-list 'auto-mode-alist '("\\.tsx?$" . web-mode))
  (add-hook 'web-mode-hook  'web-mode-init-hook)
  :custom
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-content-types-alist '(("tlsx" . "\\.ts[x]?\\'")))
  )


(use-package vi-tilde-fringe
  :init
  (fringe-mode)
  :config
  (add-hook 'prog-mode-hook 'vi-tilde-fringe-mode))

(use-package diff-hl
  :config
  (diff-hl-mode))

;; code formatting
(use-package apheleia
  :config
  (add-to-list 'apheleia-mode-alist '(typescriptreact-mode . prettier-typescript))
  (apheleia-global-mode +1)
  )


(use-package avy
  :config
  (global-set-key (kbd "C-z") 'avy-goto-char-2))

;; ew
(use-package vue-mode)

;; irc stuff
(setq url-proxy-services
      '(("http"     . "proxy2.iiit.ac.in:80")))

(setq erc-server "irc.libera.chat"
      erc-nick "komikat"    ; Change this!
      erc-user-full-name "Akshit Kumar"  ; And this!
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.libera.chat" "#haskell" "#emacs"))
      erc-kill-buffer-on-part t
            erc-auto-query 'bury)
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; dont use these anymore tbvh

;; tree sitter
;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

(use-package typescript-mode
  :after tree-sitter
  :config
  ;; we choose this instead of tsx-mode so that eglot can automatically figure out language for server
  ;; see https://github.com/joaotavora/eglot/issues/624 and https://github.com/joaotavora/eglot#handling-quirky-servers
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

(use-package tsi
  :after tree-sitter
  :straight (tsi :host github :repo "orzechowskid/tsi.el")
  ;; define autoload definitions which when actually invoked will cause package to be loaded
  :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
  :init
  (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
  (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
  (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
  (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

(setq tsi-typescript-indent-offset 4)

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)        
   ("C-;" . embark-dwim)       
   ("C-h B" . embark-bindings)))

(use-package org-download)
(add-hook 'dired-mode-hook 'org-download-enable)

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package slime)
(setq inferior-lisp-program "sbcl")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(apheleia-formatters
   '((bean-format "bean-format")
	 (black "black" "-")
	 (brittany "brittany")
	 (clang-format "clang-format" "-assume-filename"
				   (or
					(buffer-file-name)
					(cdr
					 (assoc major-mode
							'((c-mode . ".c")
							  (c++-mode . ".cpp")
							  (cuda-mode . ".cu")
							  (protobuf-mode . ".proto"))))
					".c"))
	 (crystal-tool-format "crystal" "tool" "format" "-")
	 (dart-format "dart" "format")
	 (elm-format "elm-format" "--yes" "--stdin")
	 (fish-indent "fish_indent")
	 (gofmt "gofmt")
	 (gofumpt "gofumpt")
	 (goimports "goimports")
	 (google-java-format "google-java-format" "-")
	 (isort "isort" "-")
	 (lisp-indent . apheleia-indent-lisp-buffer)
	 (ktlint "ktlint" "--stdin" "-F")
	 (latexindent "latexindent" "--logfile=/dev/null")
	 (mix-format "mix" "format" "-")
	 (nixfmt "nixfmt")
	 (ocamlformat "ocamlformat" "-" "--name" filepath "--enable-outside-detected-project")
	 (phpcs "apheleia-phpcs")
	 (prettier npx "prettier" "--stdin-filepath" filepath)
	 (prettier-css npx "prettier" "--stdin-filepath" filepath "--parser=css")
	 (prettier-html npx "prettier" "--stdin-filepath" filepath "--parser=html")
	 (prettier-graphql npx "prettier" "--stdin-filepath" filepath "--parser=graphql")
	 (prettier-javascript npx "prettier" "--stdin-filepath" filepath "--parser=babel-flow")
	 (prettier-json npx "prettier" "--stdin-filepath" filepath "--parser=json")
	 (prettier-markdown npx "prettier" "--stdin-filepath" filepath "--parser=markdown")
	 (prettier-ruby npx "prettier" "--stdin-filepath" filepath "--parser=ruby")
	 (prettier-scss npx "prettier" "--stdin-filepath" filepath "--parser=scss")
	 (prettier-typescript npx "prettier" "--tab-width 4" "--use-tabs" "--stdin-filepath" filepath "--parser=typescript")
	 (prettier-yaml npx "prettier" "--stdin-filepath" filepath "--parser=yaml")
	 (shfmt "shfmt" "-i" "4")
	 (stylua "stylua" "-")
	 (rustfmt "rustfmt" "--quiet" "--emit" "stdout")
	 (terraform "terraform" "fmt" "-")))
 '(tsi-typescript-indent-offset 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:family "Helvetica Neue" :height 180 :weight Light)))))
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
