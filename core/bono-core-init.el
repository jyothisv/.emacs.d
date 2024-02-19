;; -*- lexical-binding: t; -*-

;; Inspired by https://github.com/jimeh/.emacs.d

;; Compile all elisp on-demand
(setq native-comp-jit-compilation t)
(setq native-compile-prune-cache t)		;Prune old versions

(defvar bono-core-dir (file-name-directory load-file-name)
  "Core directory for the bono Emacs configuration.")

(defvar bono-dir (expand-file-name ".." bono-core-dir)
  "Root directory for the bono Emacs configuration.")

(defvar bono-cache-dir (expand-file-name "cache" user-emacs-directory)
  "The cache directory for the bono Emacs configuration.")

;; Create the cache directory if it doesn't exist
(unless (file-exists-p bono-cache-dir)
  (make-directory bono-cache-dir))

(defun bono-dir (name)
  "Return the abolute path of NAME under the bono configuration root directory."
  (expand-file-name name bono-dir))

(defun bono-cache-dir (name)
  "Return the abolute path of NAME under the bono configuration cache directory."
  (expand-file-name name bono-cache-dir))


;; repeat mode
(repeat-mode +1)

;; Set the load path and load core modules
(add-to-list 'load-path bono-core-dir)

(require 'bono-core-utils)
(message "bono-core-utils initialized in %s" (emacs-init-time))

(require 'bono-core-dynamic-utils)
(message "bono-core-dynamic-utils initialized in %s" (emacs-init-time))

(require 'bono-core-packages)
(message "bono-core-packages initialized in %s" (emacs-init-time))

(require 'bono-core-theme)
(message "bono-core-theme initialized in %s" (emacs-init-time))

(require 'bono-core-ui)
(message "bono-core-ui initialized in %s" (emacs-init-time))

(require 'bono-core-editor)
(message "bono-core-editor initialized in %s" (emacs-init-time))

(require 'bono-core-search)
(message "bono-core-search initialized in %s" (emacs-init-time))

(require 'bono-core-fm)
(message "bono-core-fm initialized in %s" (emacs-init-time))

(require 'bono-core-modes)
(message "bono-core-modes initialized in %s" (emacs-init-time))

(message "Emacs initialized in %s" (emacs-init-time))

(provide 'bono-core-init)
;;; bono-core-init.el ends here
