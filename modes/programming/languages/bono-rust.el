;; -*- lexical-binding: t; -*-

(use-package rust-ts-mode
  :config
  (add-hook 'rust-ts-mode-hook 'eglot-ensure)
  (add-hook 'rust-ts-mode-hook 'eglot-format-buffer-on-save)
  )


(provide 'bono-rust)
