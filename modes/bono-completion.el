;; -*- lexical-binding: t; -*-

;; Vertico
(use-package vertico
  :init
  (vertico-mode)

  :bind (:map vertico-map
              ("M-q" . #'vertico-quick-insert)
              ("C-q" . #'vertico-quick-exit))

  :config
  ;; From https://github.com/minad/vertico
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)

  ;; (setq vertico-multiform-categories
  ;;       '((file grid)
  ;;         (t grid)))

  ;; (setq vertico-multiform-commands
  ;;       '(
  ;;         (consult-buffer unobtrusive)
  ;;         (execute-extended-command flat)
  ;;         ))
  ;; (vertico-multiform-mode)
  )

;; save history for vertico
(use-package savehist
  :init
  (savehist-mode)
  :config
  (setq history-length 10000)
  (setq history-delete-duplicates t)
  ;; (setq savehist-file (bono-cache-dir "savehist"))
  )


;; vertico directory
(use-package vertico-directory
  :after vertico
  :straight nil
  :bind (:map vertico-map
              ("RET" . #'vertico-directory-enter)
              ("M-h" . #'vertico-directory-delete-word)
              ("DEL" . #'vertico-directory-delete-char)
              ("M-DEL" . #'vertico-directory-delete-word)))


(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :config
  (marginalia-mode))


;; Consult
(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("C-x f" . consult-recent-file)
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("M-`" . consult-register-load)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.

  (setq consult-preview-key "M-.")

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   :preview-key (list :debounce 0.5 "<up>" "<down>")
   )


  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; From https://manueluberti.eu/emacs/2022/09/01/consult-xref/
  (defun bono-project-find-regexp ()
    "Use `project-find-regexp' with completion."
    (interactive)
    (defvar xref-show-xrefs-function)
    (let ((xref-show-xrefs-function #'consult-xref))
      (if-let ((tap (thing-at-point 'symbol)))
          (project-find-regexp tap)
        (call-interactively #'project-find-regexp))))

  )

;; (use-package corfu
;;   ;; Optional customizations
;;   :custom
;;   ;; (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   ;; (corfu-separator ?\s)          ;; Orderless field separator
;;   ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
;;   (corfu-preview-current nil)
;;   (corfu-preselect-first t)
;;   ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
;;   (corfu-echo-documentation t)
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; Enable Corfu only for certain modes.
;;   ;; :hook ((prog-mode . corfu-mode)
;;   ;;        (shell-mode . corfu-mode)
;;   ;;        (eshell-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since Dabbrev can be used globally (M-/).
;;   ;; See also `corfu-excluded-modes'.
;;   :bind
;;   (:map corfu-map
;;         ("M-h" . nil)
;;         ("#" . #'corfu-insert-separator)
;;         ;; ("TAB" . #'corfu-next)
;;         ;; ([tab] . #'corfu-next)
;;         ;; ("S-TAB" . #'corfu-previous)
;;         ;; ("[backtab]" . #'corfu-previous)
;;         ("<f1>" . #'corfu-info-documentation))
;;   :init
;;   (global-corfu-mode)
;;   (corfu-history-mode 1)
;;   (add-to-list 'savehist-additional-variables 'corfu-history))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  )


;; Kind-icons
;; (use-package kind-icon
;;   :after corfu
;;   :custom
;;   (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
;;   :config
;;   (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Embark
(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ;; ("M-." . embark-dwim)        ;; good alternative: M-.
   ;; ("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'

   )

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))



;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))




(use-package avy
  :bind ("C-'" . avy-goto-char-timer))


;; Autocomplete: Company
(use-package company
  :diminish company-mode
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-code-everywhere t)
  (setq company-dabbrev-minimum-length 1)
  (add-hook 'after-init-hook 'global-company-mode)
  ;; C-h should delete-backward-char even when company mode is active.
  (define-key company-active-map (kbd "C-h") 'delete-backward-char)
  ;; C-n and C-p for going through the company suggestions
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous) )


(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1)
  (setq company-prescient-sort-length-enable nil))


(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (yas-global-mode 1))


(provide 'bono-completion)
