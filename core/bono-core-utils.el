;; -*- lexical-binding: t; -*-

(defun quick-switch-to-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

;; From https://github.com/jimeh/.emacs.d/blob/master/core/siren-core-utils.el
(defun bono-recursive-add-to-load-path (dir)
  "Add `dir' and its subdirectories to the load path"
  (add-to-list 'load-path dir)
  (dolist (f (directory-files dir))
    (let ((name (expand-file-name f dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (bono-recursive-add-to-load-path name)))))


(defun bono-rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting.

Borrowed from: http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)))))))


;; Open Line similar to the 'o' key in Vim
(defun vim-open-line-below (arg)
  (interactive "p")
  (if (> arg 0)
                                        ; if arg is positive, move to the end of line
      (move-end-of-line 1)
                                        ; otherwise, go to the beginning of the line
    (move-beginning-of-line 1))
                                        ; insert |arg| newlines
  (newline (abs arg))
                                        ; if arg is +ve, we're already at one
                                        ; level down, so decrement arg
                                        ; accordingly
  (if (> arg 0) (setq arg (1- arg)))

  (forward-line arg)
  (indent-according-to-mode))           ; Finally, indent

(defun vim-open-line-above (arg)
  (interactive "p")
  (vim-open-line-below (- arg)))


(defun create-directory-if-needed ()
  "Creates the directory of the current file if it doesn't exist already"
  (when buffer-file-name                ;if this buffer is associated with a file
    (let ((dir default-directory))      ;we use dir for potential future modification
      (if (not (file-exists-p dir))
          (make-directory dir t)))))


(defun gosmacs-transpose-chars (n)
  "Always tranpose chars before point.
With a numeric argument n, transpose the characters at `(point) - n - 1' and `(point) - n'.
Does not move the point."
  (interactive "*p")
  (let ((pt (point)))
    (unwind-protect
        (when (> (- pt n) (point-min))
          (forward-char (- n))
          (transpose-chars 1))
      (goto-char pt))))


(provide 'bono-core-utils)
