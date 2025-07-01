;;; Outline-mode

;; Configure outline-minor-mode for programming languages
(defun my/setup-outline-minor-mode ()
  "Setup outline-minor-mode with custom configuration."
  (outline-minor-mode 1)
  (setq-local outline-blank-line t))

;; Python outline configuration
(defun my/python-outline-setup ()
  "Configure outline-minor-mode for Python files."
  (setq-local outline-regexp "#+\\|def \\|class ")
  (setq-local outline-level
              (lambda ()
                (cond
                 ((looking-at "###") 3)
                 ((looking-at "##") 2)
                 ((looking-at "#") 1)
                 ((looking-at "class ") 1)
                 ((looking-at "def ") 2)
                 (t 1))))
  (my/setup-outline-minor-mode))

;; Clojure/ClojureScript outline configuration
(defun my/clojure-outline-setup ()
  "Configure outline-minor-mode for Clojure/ClojureScript files."
  (setq-local outline-regexp ";+\\|(defn\\|(defmacro\\|(def ")
  (setq-local outline-level
              (lambda ()
                (cond
                 ((looking-at ";;;") 3)
                 ((looking-at ";;") 2)
                 ((looking-at ";") 1)
                 ((looking-at "(defn\\|(defmacro") 1)
                 ((looking-at "(def") 2)
                 (t 1))))
  (my/setup-outline-minor-mode))

;; Apply outline configuration to appropriate modes
(add-hook 'python-mode-hook #'my/python-outline-setup)
(add-hook 'clojure-mode-hook #'my/clojure-outline-setup)
(add-hook 'clojurescript-mode-hook #'my/clojure-outline-setup)
