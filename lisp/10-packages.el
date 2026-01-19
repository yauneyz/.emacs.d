;;; 10-packages.el --- straight.el, use-package, etc. -*- lexical-binding: t; -*-

;; NOTE: package.el is disabled in favor of straight.el
;; early-init.el sets package-enable-at-startup to nil

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

(with-eval-after-load 'straight
  ;; Some packages still `(require 'package)' for metadata; we intentionally
  ;; keep package.el disabled so suppress its redundant warning.
  (setq straight-package--warning-displayed t))

(with-eval-after-load 'package
  ;; Never allow `package-initialize' to activate ELPA archives; straight.el
  ;; owns package management for this config.
  (advice-add 'package-initialize :override #'ignore))

(setq straight-use-package-by-default t)

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
;; User info (email, name)
;; --------------------------------------------------------------------------
(setq user-full-name     (or (getenv "GIT_AUTHOR_NAME") "Zac Yauney")
      user-mail-address  (or (getenv "GIT_AUTHOR_EMAIL") "zac.yauney@gmail.com"))

(provide 'packages)
;;; 10-packages.el ends here
