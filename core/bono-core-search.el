;; -*- lexical-binding: t; -*-

;; Isearch-mode still considers C-h to be the help char.
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

(use-package anzu
  :bind
  ([remap query-replace] . anzu-query-replace-regexp))


(provide 'bono-core-search)
