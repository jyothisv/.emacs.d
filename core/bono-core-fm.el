;; -*- lexical-binding: t; -*-

;; File management

;; Make the file executable if it is a script
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; Create parent directories if needed
(add-hook 'before-save-hook 'create-directory-if-needed)


;; save minibuffer history
(use-package savehist
  :init
  (savehist-mode)
  :config
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  (setq savehist-file (bono-cache-dir "savehist"))
  )



(use-package recentf
  :config
  (recentf-mode 1)
  ;; (setq recentf-save-file (bono-cache-dir "recentf"))
  (setq recentf-max-saved-items 10000))

;; Auto-follow symlinks to files under version control
(setq vc-follow-symlinks t)

(global-set-key (kbd "C-c f r") 'rename-visited-file)

(setq switch-to-prev-buffer-skip-regexp "\\*")

(provide 'bono-core-fm)
