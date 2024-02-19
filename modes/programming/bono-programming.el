;; -*- lexical-binding: t; -*-

;; line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; From https://gist.github.com/carlosrogue/777f43b4a46400cae21aaf9ba5ca5ccc
;; -10 to place this before eglot's willSave notification
(defun eglot-format-buffer-on-save ()
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  (add-to-list 'eglot-server-programs
               '(rust-ts-mode . ("rust-analyzer"
                                 :initializationOptions (:check (:command "clippy")))))
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
(setq compilation-auto-jump-to-first-error 'if-location-known)

;; Compile command
(setq compile-command "")

(global-set-key (kbd "C-<f9>") 'compile)
(global-set-key (kbd "<f9>") 'recompile)

;; TODO: Enable conflict-free recompilation in different directories
;; (make-variable-buffer-local 'compilation-directory)

(use-package flymake
  :config
  (define-key flymake-mode-map (kbd "M-g n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-g p") 'flymake-goto-prev-error)
  (setq flymake-mode-line-lighter "")   ;Do not show anything in the mode-line
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
