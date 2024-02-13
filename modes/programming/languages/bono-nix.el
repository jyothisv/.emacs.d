;; -*- lexical-binding: t; -*-

;; Nix mode
(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (add-hook 'nix-mode-hook 'eglot-ensure))


(provide 'bono-nix)
