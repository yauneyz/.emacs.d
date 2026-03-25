;;; 26-xref.el --- Modern xref navigation -*- lexical-binding: t; -*-

(declare-function lsp-find-definition "lsp-mode" (&key display-action))
(declare-function lsp-find-references "lsp-mode" (&optional exclude-declaration &key display-action))

(use-package xref
  :straight (:type built-in)
  :init
  (autoload 'consult-xref "consult-xref" nil t)
  :custom
  (xref-search-program 'ripgrep)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  (with-eval-after-load 'consult
    (consult-customize
     consult-xref
     :preview-key '(:debounce 0.2 any))))

(defun +xref/find-definition ()
  "Jump to the LSP definition when available, otherwise fall back to xref."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (call-interactively #'lsp-find-definition)
    (call-interactively #'xref-find-definitions)))

(defun +xref/find-references ()
  "Jump to the LSP references when available, otherwise fall back to xref."
  (interactive)
  (if (bound-and-true-p lsp-mode)
      (call-interactively #'lsp-find-references)
    (call-interactively #'xref-find-references)))

(provide '26-xref)
;;; 26-xref.el ends here
