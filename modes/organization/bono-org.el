;; -*- lexical-binding: t; -*-

(use-package org
  :straight nil
  :config
  (setq org-edit-src-content-indentation 0)
  (setq org-startup-folded 'content)
  (setq org-directory "~/org"))

(use-package org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-mode) )

(provide 'bono-org)
