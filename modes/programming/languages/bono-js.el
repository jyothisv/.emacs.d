;; -*- lexical-binding: t; -*-

(setq js-indent-level 2)

(use-package prettier
  :hook
  (js-ts-mode . prettier-mode)
  (web-mode . prettier-mode)
  )

(provide 'bono-js)
