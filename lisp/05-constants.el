;;; 05-constants.el --- OS / tool probes  -*- lexical-binding: t; -*-

(defconst *sys/linux*  (eq system-type 'gnu/linux)   "Running on GNU/Linux?")
(defconst *sys/win32*  (eq system-type 'windows-nt)  "Running on Windows?")

(defconst python-p (or (executable-find "python3")
                       (and (executable-find "python")
                            (string-match "Python 3"
                                          (shell-command-to-string "python --version"))))
  "Is python3 available?")

(defconst pip-p (or (executable-find "pip3")
                    (and (executable-find "pip")
                         (string-match "python 3"
                                       (shell-command-to-string "pip --version"))))
  "Is pip3 available?")

(defconst clangd-p (or (executable-find "clangd")
                       (executable-find "/usr/local/opt/llvm/bin/clangd"))
  "Is clangd available?")

(defconst eaf-env-p (and (display-graphic-p) python-p pip-p)
  "Do we have what EAF needs?")


(provide '05-constants)
