;;; complete.el --- setup completion packages

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

(provide 'complete)

