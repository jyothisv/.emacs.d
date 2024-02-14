;; -*- lexical-binding: t; -*-

(setq-default fill-column 80
              sentence-end-double-space nil ; Single space after the dot.
              )


;; When pasting, delete the active region, but only for mouse/shift selections
(setq delete-selection-temporary-region 'selection)
(delete-selection-mode 1)
;; I don't want to type yes/no
(fset 'yes-or-no-p 'y-or-n-p)                                    ;From MasteringEmacs
(global-subword-mode 1)

;; Seperate tab and C-i
(define-key input-decode-map "\C-i" [C-i])

;; Auto-save-directory
(setq backup-directory-alist
      `(("." . ,(bono-cache-dir "backup")))
      tramp-backup-directory-alist backup-directory-alist)

;; delete old back-ups silently
(setq delete-old-versions t)

(setq-default ispell-program-name "aspell")

;; enable more than one simultaneous active minibuffers
(setq enable-recursive-minibuffers t)

;; For easy copying from read only buffers
(setq kill-read-only-ok t)

;; Disable restrictions for invoking specific commands
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq-default indent-tabs-mode nil)

;; Don't ring bell -- just show a message
(setq ring-bell-function #'(lambda () (message "*BEEP*")))

(setq comint-prompt-read-only t)

;; Line numbers
(setq-default
 display-line-numbers-type 'relative
 display-line-numbers-width 2)

;; Show completions for the currently typed prefix-key
(use-package which-key
  :diminish
  :config (which-key-mode))

;; Clean the whitespace before saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Some keybindings
;; help function is already bound to F1. So C-h can be assigned to backspace.
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; Kill up to, but not including ARGth occurrence of CHAR.
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; C-S-k for delete whole line
(global-set-key (kbd "C-S-k") 'kill-whole-line)

;; I never press the insert key intentionally
(global-unset-key [insert])

;; For quickly switching to the last two buffers
(global-set-key [f2] 'quick-switch-to-last-buffer)

;; open line as in vim
(define-key global-map (kbd "C-o") 'vim-open-line-below)
(define-key global-map (kbd "C-S-o") 'vim-open-line-above)

;; transpose the last two chars wherever you are.
(global-set-key (kbd "C-t") 'gosmacs-transpose-chars)

;; Join the current line with the next line. From Magnar Sveen.
(global-set-key (kbd "C-M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

;; When duplicating line/region, move the point to the first line of the duplicate line/region
(setq duplicate-line-final-position 1)
(global-set-key (kbd "M-<down>") #'duplicate-dwim)


;; Mouse settings
;; Allow dragging and dropping text to other programs
(setq mouse-drag-and-drop-region-cross-program t)
;; When using mouse to select text, automatically scroll when needed
(setq mouse-drag-and-drop-region-scroll-margin t)

(provide 'bono-core-editor)

