;;; style.el --- styles for my emacs config

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
                show-paren-mode))
  (funcall mode t))
(dolist (mode '(scroll-bar-mode
                tooltip-mode
                tool-bar-mode
                global-hl-line-mode))
  (funcall mode -1))
(fringe-mode 1)
(load-theme 'modus-vivendi t)
(add-to-list 'default-frame-alist
             '(font . "-*-SF Mono-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1"))

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


(provide 'style)
