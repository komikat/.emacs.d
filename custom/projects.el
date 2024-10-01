;;; projects.el --- setup packages for project management

(use-package projectile
  :init
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/.emacs.d/" ("~/Developer/" . 2) ("/Users/akshitkr/Library/Mobile\ Documents/com~apple~CloudDocs/Documents/stuff/" . 4)))
  :bind (:map projectile-mode-map
              ("M-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package magit
  :bind ("C-x g" . magit))

(provide 'projects)
