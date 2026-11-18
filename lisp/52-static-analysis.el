;;; 52-static-analysis.el --- Static analysis  -*- lexical-binding: t; -*-
(use-package format-all
  :commands (format-all-mode format-all-buffer)
  :hook
  ((prog-mode . +format/enable-format-all)
   ;; make sure a formatter is chosen whenever the mode enables
   (format-all-mode . format-all-ensure-formatter))
  :init
  ;; show the errors buffer only on errors (not warnings)
  (setq format-all-show-errors 'errors)
  :config
  ;; Defaults for languages (NOTE: use language names, not modes)
  (setq-default format-all-formatters
                '(("Go"                 (gofmt))
                  ("Python"             (ruff))
                  ("TypeScript"         (prettier))
                  ("Emacs Lisp"         (emacs-lisp))
                  ("Clojure/ClojureScript" (cljfmt)))))

(defun +format/enable-format-all ()
  "Enable format-all except where Apheleia owns Python formatting."
  (unless (derived-mode-p 'python-base-mode 'python-mode 'python-ts-mode)
    (format-all-mode 1)))
