;;; major.el --- major modes config
(require 'auth-source)
(use-package haskell-mode)
(use-package auctex)
(use-package pdf-tools
  :config
  (pdf-tools-install)
  :hook (pdf-view-mode . (lambda () "turn off line numbers" (display-line-numbers-mode -1))))
(use-package olivetti) ; prettier writing mode
(use-package erc
  :config
  (setopt erc-modules
          (seq-union '(sasl scrolltobottom services)
                     erc-modules))
  (setopt erc-use-auth-source-for-nickserv-password t)
  (setopt erc-nickserv-passwords
          `((libera (("komikat" . ,(auth-source-pick-first-password
                                    :host "irc.libera.chat"
                                    :user "komikat"))))))
  (setopt erc-sasl-user "komikat")
  :custom
  (erc-sasl-use-sasl t))
(provide 'major)
