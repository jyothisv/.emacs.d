;; -*- lexical-binding: t; -*-


(defvar bono-modes-dir (bono-dir "modes")
  "The root directory containing bono modes")

(bono-recursive-add-to-load-path bono-modes-dir)

;; keybindings
(require 'bono-hydra)

(require 'bono-git)

(require 'bono-projects)

(require 'bono-org)

(require 'bono-completion)

(require 'bono-term)

;; General programming settings
(require 'bono-programming)
;; Programming Languages
(require 'bono-go)
(require 'bono-js)
(require 'bono-latex)
(require 'bono-lua)
(require 'bono-nix)
(require 'bono-protobuf)
(require 'bono-python)
(require 'bono-rust)
(require 'bono-yaml)

;; AI
(require 'bono-ai)

(require 'bono-processes)

;; Direnv
(require 'bono-direnv)

(provide 'bono-core-modes)
