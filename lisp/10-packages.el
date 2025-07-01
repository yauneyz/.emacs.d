;;; 10-packages.el --- straight.el, use-package, etc. -*- lexical-binding: t; -*-

(require 'package)

;; --------------------------------------------------------------------------
;; straight.el bootstrap
;; --------------------------------------------------------------------------
(defvar bootstrap-version)
(let* ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el"
                          user-emacs-directory))
       (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t
      package-check-signature nil)

;; --------------------------------------------------------------------------
;; package-archives (still used for built-ins like gpg keys, etc.)
;; --------------------------------------------------------------------------
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")
        ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; --------------------------------------------------------------------------
;; use-package
;; --------------------------------------------------------------------------
(straight-use-package 'use-package)
(eval-and-compile
  (setq use-package-verbose              t
        use-package-expand-minimally     t
        use-package-compute-statistics    t
        use-package-enable-imenu-support  t))
(eval-when-compile
  (require 'use-package)
  (require 'bind-key))
(setq use-package-always-ensure t)


;; --------------------------------------------------------------------------
;; Convenience helper used later
;; --------------------------------------------------------------------------
(defun install-if-necessary (package)
  "Install PACKAGE unless it is already installed."
  (unless (package-installed-p package)
    (package-install package)))

;; --------------------------------------------------------------------------
;; User info (email, name)
;; --------------------------------------------------------------------------
(setq user-full-name     (or (getenv "GIT_AUTHOR_NAME") "Zac Yauney")
      user-mail-address  (or (getenv "GIT_AUTHOR_EMAIL") "zac.yauney@gmail.com"))

(provide 'packages)
;;; 10-packages.el ends here
