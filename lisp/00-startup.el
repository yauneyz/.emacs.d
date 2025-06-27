;;; 00-startup.el --- GC & focus hooks  -*- lexical-binding: t; -*-

;; Better GC during normal use -------------------------------------------------
(defvar better-gc-cons-threshold 134217728   ; 128 MB
  "Default `gc-cons-threshold' once startup completes.")

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold better-gc-cons-threshold
                  file-name-handler-alist file-name-handler-alist-original)
            (makunbound 'file-name-handler-alist-original)))

;; Collect when Emacs is in the background or after minibuffer -----------------
(add-hook 'emacs-startup-hook
          (lambda ()
            (if (boundp 'after-focus-change-function)
                (add-function
                 :after after-focus-change-function
                 (lambda () (unless (frame-focus-state) (garbage-collect))))
              (add-hook 'focus-out-hook #'garbage-collect))

            (defun gc-minibuffer-setup-hook ()
              (setq gc-cons-threshold (* better-gc-cons-threshold 2)))
            (defun gc-minibuffer-exit-hook ()
              (garbage-collect)
              (setq gc-cons-threshold better-gc-cons-threshold))
            (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
            (add-hook 'minibuffer-exit-hook  #'gc-minibuffer-exit-hook)))

(provide 'startup)
;;; 00-startup.el ends here
