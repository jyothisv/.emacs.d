;; -*- lexical-binding: t; -*-

(use-package gptel
  :init
  (gptel-make-ollama "Ollama"
    :host "localhost:11434"
    :stream t
    :models '("llama3.1:8b-instruct-q8_0")))

(provide 'bono-ai)
