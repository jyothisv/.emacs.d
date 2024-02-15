;; -*- lexical-binding: t; -*-

(use-package org
  ;; :straight nil
  :config
  (setq org-edit-src-content-indentation 0)
  (setq org-startup-folded 'content)
  (setq org-directory "~/org")
  :bind
  (:map org-mode-map
        ("M-h" . #'backward-kill-word))
  )

(use-package org-modern
  :custom
  (org-modern-hide-stars " ")           ;Hide the leading stars
  :config
  (add-hook 'org-mode-hook #'org-modern-mode) )

(use-package org-roam
  :custom
  (org-roam-db-location (bono-cache-dir "org-roam.db"))
  (org-roam-directory (file-truename "~/org/roam"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(provide 'bono-org)
