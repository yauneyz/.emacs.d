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

;; --- New configuration for programming modes ---
(defun my/prog-olivetti-setup ()
  "Configure Olivetti in programming modes: set a 100-column width and disable visual margins."
  (setq-local olivetti-body-width 100)
  (setq-local olivetti-minimum-body-width 100)
  ;; Disable visual margins by setting the style to nil.
  ;; (See Olivetti’s documentation: setting `olivetti-style` to nil turns off any
  ;; extra side margins.)
  (setq-local olivetti-style nil)
  (olivetti-mode 1))

(dolist (hook '(prog-mode-hook
                go-mod-mode-hook
                json-mode-hook
                protobuf-mode-hook
                protobuf-ts-mode-hook))
  ;; Proto hooks may not be defined yet (autoloaded modes), so bind them before use.
  (unless (boundp hook)
    (set hook nil))
  (add-hook hook #'my/prog-olivetti-setup))

(provide '51-lang-bundle)
;;; 51-lang-bundle.el ends here
