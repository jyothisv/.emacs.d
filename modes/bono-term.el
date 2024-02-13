;; -*- lexical-binding: t; -*-

(use-package vterm
  :bind
  (
   :map vterm-mode-map
   ([f2] . #'quick-switch-to-last-buffer)
   ([f12] . #'vterm-toggle)
   ))

(use-package vterm-toggle
  :config
  (global-set-key [f12] 'vterm-toggle)
  (global-set-key [C-f12] 'vterm-toggle-cd)

  ;; Projectile integration
  (setq vterm-toggle-scope 'project
        vterm-toggle-project-root t)

  ;; cd to the directory where your previous buffer file exists
  (define-key vterm-mode-map [(control return)]   #'vterm-toggle-insert-cd)

  ;; Show the vterm buffer at the bottom
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 ;; (display-buffer-reuse-window display-buffer-at-bottom)
                 (display-buffer-reuse-window display-buffer-in-direction)
                 ;;display-buffer-in-direction/direction/dedicated is added in emacs27
                 (direction . bottom)
                 (dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3)))

  ;; a dedicated vterm for compiling
  ;; (setq vterm-toggle-use-dedicated-buffer t)

  (defvar vterm-compile-dedicated-buffer nil)
  (defun vterm-compile ()
    (interactive)
    (let ((vterm-toggle-use-dedicated-buffer t)
          (vterm-toggle--vterm-dedicated-buffer vterm-compile-dedicated-buffer))
      (with-current-buffer (vterm-toggle-cd)
        (setq vterm-compile-dedicated-buffer (current-buffer))
        (rename-buffer "term compile")
        (compilation-shell-minor-mode 1)
        (vterm-send-string compile-command t)
        (vterm-send-return))
      )
    )
  )

(provide 'bono-term)
