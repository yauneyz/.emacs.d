;;; 51-lang-bundle.el --- Aggregate language modules -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads all language-specific modules while providing shared prog-mode tweaks
;; that do not belong to a single language.

;;; Code:

(require 'lang-clojure)
(require 'lang-python)
(require 'lang-go)
(require 'lang-typescript)
(require 'lang-rust)
(require 'lang-haskell)

;; Shared programming conveniences
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom (highlight-indent-guides-method 'character))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(defun +lang/trim-trailing-whitespace ()
  "Keep buffers tidy without requiring ws-butler."
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(add-hook 'prog-mode-hook #'+lang/trim-trailing-whitespace)

(provide '51-lang-bundle)
;;; 51-lang-bundle.el ends here
