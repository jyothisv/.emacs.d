;; -*- lexical-binding: t; -*-

(use-package rust-ts-mode
  :config
  (add-hook 'rust-ts-mode-hook 'eglot-ensure)
  (add-hook 'before-save-hook 'eglot-format-buffer)
  )


(provide 'bono-rust)
