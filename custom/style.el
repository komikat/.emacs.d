;;; style.el --- basic styling for emacs

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

(provide 'style)
