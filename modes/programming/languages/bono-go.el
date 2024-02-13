;; -*- lexical-binding: t; -*-

(use-package go-ts-mode
  :straight nil
  :config
  (add-hook 'go-ts-mode-hook 'eglot-ensure)
  (add-hook 'before-save-hook 'eglot-format-buffer)
  )



(provide 'bono-go)
