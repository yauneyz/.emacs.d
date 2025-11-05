(use-package format-all
  :commands (format-all-mode format-all-buffer)
  :hook
  ((prog-mode . format-all-mode)
   ;; make sure a formatter is chosen whenever the mode enables
   (format-all-mode . format-all-ensure-formatter))
  :init
  ;; show the errors buffer only on errors (not warnings)
  (setq format-all-show-errors 'errors)
  :config
  ;; Defaults for languages (NOTE: use language names, not modes)
  (setq-default format-all-formatters
                '(("Go"                 (gofmt))
                  ("Python"             (black))
                  ("TypeScript"         (prettier))
                  ("Emacs Lisp"         (emacs-lisp))
                  ("Clojure/ClojureScript" (cljfmt)))))
