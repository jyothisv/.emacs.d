;; -*- lexical-binding: t; -*-

(use-package auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t)
  (setq-default Tex-master nil)
  (setq TeX-source-correlate-mode t)


  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'flymake-mode)
  
  ;; set up zathura as the pdf viewer
  (setq TeX-output-view-style
        '(("^pdf$" "." "zathura %o")
          ("^html?$" "." "firefox %o")))
  )

(provide 'bono-latex)
