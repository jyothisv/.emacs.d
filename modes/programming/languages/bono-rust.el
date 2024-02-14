;; -*- lexical-binding: t; -*-

(use-package rust-ts-mode
  :config
  (add-hook 'rust-ts-mode-hook 'eglot-ensure)
  )


(provide 'bono-rust)
