
;; Folding
;;; 52-folding.el --- Origami + persistence + Evil keys  -*- lexical-binding: t; -*-

;; Packages:
;; - origami: folding engine with recursive ops
;; - lsp-origami: use LSP folding ranges when present
;; - savefold: persist folds to disk (supports origami)
;; - evil (already in your setup)

(use-package origami
  :ensure t
  :init
  (require 'origami-parsers) ; make sure parsers are defined early
  :hook ((prog-mode . origami-mode))
  :config
  (dolist (pair '(
                  ;; Clojure family → Lisp parser specialized for clj
                  (clojure-mode         . origami-clj-parser)
                  (clojurescript-mode   . origami-clj-parser)
                  (clojurec-mode        . origami-clj-parser)

                  ;; Emacs Lisp
                  (emacs-lisp-mode       . origami-elisp-parser)
                  (lisp-interaction-mode . origami-elisp-parser)

                  ;; Python
                  (python-mode          . origami-python-parser)
                  (python-ts-mode       . origami-python-parser)

                  ;; Go / JS / TS → brace/c-style parser
                  (go-mode              . origami-c-style-parser)
                  (go-ts-mode           . origami-c-style-parser)
                  (js-mode              . origami-c-style-parser)
                  (js-ts-mode           . origami-c-style-parser)
                  (typescript-mode      . origami-c-style-parser)
                  (typescript-ts-mode   . origami-c-style-parser)
                  (tsx-ts-mode          . origami-c-style-parser)))
    (add-to-list 'origami-parser-alist pair)))

;; ;; LSP folding ranges → Origami nodes when LSP is active
;; (use-package lsp-origami
;;   :after (lsp-mode origami)
;;   :ensure t
;;   :hook (lsp-after-open . lsp-origami-try-enable))

;; Persist folds across sessions (supports origami/hideshow/outline etc.)
(use-package savefold
  :ensure t
  :init
  ;; Persist Origami folds; you can add 'outline 'org 'hideshow later if desired.
  (setq savefold-backends '(origami)
        savefold-directory (locate-user-emacs-file "savefold"))
  :config
  (savefold-mode 1))

(provide '52-folding)
;;; 52-folding.el ends here
