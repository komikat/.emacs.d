;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;; straight, vertico - marginalia - orderless - consult, company, projectile
;;; Code:

(add-to-list 'load-path (expand-file-name "custom" user-emacs-directory))

(require 'boot)              ; bootstrap straight + use-package + path
(require 'style)             ; theme, fonts, fringe and friends
(require 'complete)          ; completion packages
(require 'projects)          ; projectile + magit
(require 'utils)             ; snippets, formatters, other utils
(require 'major)             ; major modes

(provide 'init)
;;; init.el ends here
