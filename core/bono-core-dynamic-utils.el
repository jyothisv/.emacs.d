;; Clean the whitespace empty lines at the beginning/end, and trailing
;; whitespace and missing newline at the end of file but nothing else
(defun bono-whitespace-cleanup ()
  (interactive)
  (let ((whitespace-style '(trailing empty mark missing-newline-at-eof)))
    (message "inside bono whitespace cleanup")
    (whitespace-cleanup))
  )


(provide 'bono-core-dynamic-utils)
