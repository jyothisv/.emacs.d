;; -*- lexical-binding: t; -*-

;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default)
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (define-key projectile-command-map (kbd "s") 'consult-ripgrep)
  (define-key projectile-command-map (kbd "P") #'projectile-toggle-between-implementation-and-test)
  (define-key projectile-command-map (kbd "t") #'projectile-test-project)

  (setq projectile-cache-file (bono-cache-dir "projectile.cache")
        projectile-known-projects-file (bono-cache-dir "projectile-bookmarks.eld"))
  )

(provide 'bono-projects)
