;; -*- lexical-binding: t; -*-

(setq-default recenter-positions '(5 top bottom)
              scroll-conservatively 101 ; Avoid recentering
              scroll-margin 2
              scroll-preserve-screen-position t)

;; default to better frame titles
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b")))) ; "   %n (%i)"))


(blink-cursor-mode -1)                  ; Do not blink the cursor

;; Enable the tab-bar
(tab-bar-mode)

;; (setq tab-bar-new-tab-choice 'window)
(setq tab-bar-new-tab-to 'rightmost)

;; C-c <left>/<right> for cycling through tab bars
(tab-bar-history-mode +1)

;; Keys for tab navigation
(global-set-key (kbd "M-<escape>") #'tab-recent)
(global-set-key (kbd "M-<f1>") #'(lambda () (interactive) (tab-select 1)))
(global-set-key (kbd "M-<f2>") #'(lambda () (interactive) (tab-select 2)))
(global-set-key (kbd "M-<f3>") #'(lambda () (interactive) (tab-select 3)))
(global-set-key (kbd "M-<f4>") #'(lambda () (interactive) (tab-select 4)))
(global-set-key (kbd "M-<f5>") #'(lambda () (interactive) (tab-select 5)))



(pixel-scroll-precision-mode)

;; Setting font(s)
(defvar bono-default-font-name)
(defvar bono-font-height)
(when (eq system-type 'gnu/linux)
  (setq bono-default-font-name "JetBrainsMono Nerd Font")
  (setq bono-font-height 120))

(when (eq system-type 'darwin)
  (setq bono-default-font-name "Fira Code")
  (setq bono-font-height 160))

(set-face-attribute 'default nil :font bono-default-font-name :height bono-font-height)

;; For diminishing mode-line strings
(use-package diminish)

(use-package pulsar
  :config
  (pulsar-global-mode 1))

(use-package all-the-icons)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 15)
  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  (setq find-file-visit-truename t)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-buffer-modification-icon nil))


;; Help window navigation should stay in the help window
(setq help-window-keep-selected t)

(provide 'bono-core-ui)
