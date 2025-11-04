
(use-package format-all
  :config
  (setq-default format-all-formatters
                '((clojure-mode . "cljfmt")
		  (clojurescript-mode . "cljfmt")
		  (clojurec-mode . "cljfmt")
		  (python-mode . "black")
		  (typescript-mode . "prettier")
		  (emacs-lisp-mode . emacs-lisp-format))))
