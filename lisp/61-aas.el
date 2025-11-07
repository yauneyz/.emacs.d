;;; -*- lexical-binding: t; -*-

;; Helper: expand a YAS snippet by NAME, for MODE (or current major-mode)
(defun my/aas-yas-by-name (name &optional mode)
  (lambda ()
    (interactive)
    (require 'yasnippet)
    (let* ((m (or mode major-mode))                ;; <- don't shadow `mode`
           (tmpl (yas-lookup-snippet name m t)))
      (if tmpl
          (yas-expand-snippet tmpl)
        (user-error "YAS: snippet %S not found for mode %S" name m)))))

;; Macro: concise AAS → YAS-by-name declarations
(require 'cl-lib)
(defmacro aas-yas (mode &rest key-name-pairs)
  "In MODE's AAS table, bind keys to YAS snippets by name.
Usage:
  (aas-yas 'go-mode
    \";ptl\" \"println\"
    \";ptf\" \"printf\")
Pass 'global as MODE to use the global AAS table and resolve the YAS mode
at runtime from `major-mode`."
  (declare (indent 1))
  (unless (cl-evenp (length key-name-pairs))
    (error "aas-yas: need KEY NAME pairs (even number of forms)"))
  (let* ((pm (if (eq mode 'global) nil mode))
         (args (cl-loop for (k n) on key-name-pairs by #'cddr
                        append (list k `(my/aas-yas-by-name ,n ,pm)))))
    `(aas-set-snippets ,mode ,@args)))

;; Package setup
(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package aas
  :ensure t
  :after yasnippet
  :hook ((prog-mode . aas-activate-for-major-mode)
         (org-mode  . aas-activate-for-major-mode))
  :config
  ;; Global AAS table → resolve YAS mode from current buffer
  (aas-yas 'global
    ";shr" "shebang")

  ;; Org
  (aas-yas 'org-mode
    ";go" "Go code block"
    ";py" "Python code block"
    ";js" "JS code block"
    ";ts" "TS code block"
    ";ab" "Anki Basic Card"
    ";ago" "Anki Go Code Card"
    )

  ;; Go
  (aas-yas 'go-ts-mode
    ";ptl" "println"
    ";ptf" "printf"))
