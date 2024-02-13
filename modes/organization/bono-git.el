;; -*- lexical-binding: t; -*-

;; Git Gutter
(use-package git-gutter
  :ensure t
  :config
  (setq git-gutter:lighter "")          ;Show a shortened name in the modeline.
  (global-git-gutter-mode t)

  (global-set-key (kbd "M-g v") 'hydra-git-gutter/body))


;; Hydra for git-gutter
(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                      :hint nil)
  ("n" git-gutter:next-hunk "next hunk")
  ("p" git-gutter:previous-hunk "previous hunk")
  ("<" (progn (goto-char (point-min)) (git-gutter:next-hunk 1)) "first hunk")
  (">" (progn (goto-char (point-min)) (git-gutter:previous-hunk 1)) "last hunk")
  ("v" git-gutter:popup-hunk "popup hunk")
  ("<SPC>" git-gutter:mark-hunk "mark hunk")
  ("s" git-gutter:stage-hunk "stage hunk")
  ("r" git-gutter:revert-hunk "revert hunk")
  ("q" nil "quit"))


;; Git Timemachine
(use-package git-timemachine
  :ensure t)

;; Magit
(use-package magit
  :ensure t
  :commands magit-status
  :config
  (setq magit-save-some-buffers 'dontask)
  )


(provide 'bono-git)
