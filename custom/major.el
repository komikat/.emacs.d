;;; major.el --- major modes config

(use-package haskell-mode)
(use-package auctex)
(use-package pdf-tools
  :config
  (pdf-tools-install)
  :hook (pdf-view-mode . (lambda () "turn off line numbers" (display-line-numbers-mode -1))))
(use-package olivetti) ; prettier writing mode

(provide 'major)
