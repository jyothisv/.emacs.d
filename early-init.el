;; -*- lexical-binding: t; -*-
;; early-init.el.

;; Inspired by progfolio's early init file

;; We are going to use elpaca package manager
(setq package-enable-at-startup nil)
(setq inhibit-default-init nil)

;; Do not show (async) native compilation warnings
(setq native-comp-async-report-warnings-errors nil)

;; Set the directory for natively-compiled files
;; (setcar native-comp-eln-load-path (expand-file-name "cache/eln-cache" user-emacs-directory))

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)


(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defun bono-gc-after-focus-change ()
  "Run garbage collection when frame loses focus."
  (run-with-idle-timer
   5 nil
   (lambda () (unless (frame-focus-state) (garbage-collect)))))


(defun bono-reset-init-values ()
  "Reset the initial settings for the garbage collection."
  (run-with-idle-timer
   1 nil
   (lambda ()
     (setq file-name-handler-alist default-file-name-handler-alist
           gc-cons-percentage 0.1
           gc-cons-threshold 100000000)
     (message "gc-cons-threshold & file-name-handler-alist restored")
     (when (boundp 'after-focus-change-function)
       (add-function :after after-focus-change-function 'bono-gc-after-focus-change)))))

(add-hook 'after-init-hook 'bono-reset-init-values)

(menu-bar-mode -1)
(tool-bar-mode -1)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(setq frame-inhibit-implied-resize t)


(setq-default initial-scratch-message "")

;; Do not show the start-up screen and directly show the *scratch* buffer
(setq initial-buffer-choice t)

;; (save-buffers-kill-emacs)

(provide 'early-init)
