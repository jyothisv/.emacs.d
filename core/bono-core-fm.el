;; -*- lexical-binding: t; -*-

;; File management

;; Make the file executable if it is a script
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Create parent directories if needed
(add-hook 'before-save-hook 'create-directory-if-needed)

(use-package recentf
  :config
  (recentf-mode 1)
  ;; (setq recentf-save-file (bono-cache-dir "recentf"))
  (setq recentf-max-menu-items 10000))

;; Auto-follow symlinks to files under version control
(setq vc-follow-symlinks t)

(provide 'bono-core-fm)
