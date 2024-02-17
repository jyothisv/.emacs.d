;; -*- lexical-binding: t; -*-

;; line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer"
                                 :initializationOptions (:check (:command "clippy")))))
  (add-hook 'before-save-hook 'eglot-format-buffer nil t)
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename)
              ("C-c l h" . eldoc)
              ("C-c l R" . eglot-reconnect)
              ))


;; eldoc
(use-package eldoc
  :diminish eldoc-mode
  :config
  (setq eldoc-echo-area-use-multiline-p nil) ;Display only 1 line in the echo area
  )


;; Parens
(show-paren-mode 1)

;; Show some context when the matching delimiter is off screen
(setq show-paren-context-when-offscreen t)

;; Electric pair mode
(electric-pair-mode 1)

;; Rainbow parens-mode
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; comments
(use-package comment-dwim-2
  :config
  (setq cd2/region-command 'cd2/comment-or-uncomment-region)
  (setq comment-dwim-2--inline-comment-behavior 'reindent-comment)
  (global-set-key (kbd "M-;") 'comment-dwim-2))


;; Scroll the compilation output until the first error is encountered
(setq compilation-scroll-output 'first-error)


(use-package flymake
  :straight nil
  :config
  (define-key flymake-mode-map (kbd "M-g M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-g M-p") 'flymake-goto-prev-error)
  :hook
  (prog-mode . flymake-mode)
  )

;; Debugger Adapter Protocol
(use-package posframe)

;; Use treesitter mode when available
(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))


(provide 'bono-programming)
