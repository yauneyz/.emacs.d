;;; 30-editor.el --- generic editing tweaks  -*- lexical-binding: t; -*-

;; UTF-8 everywhere ------------------------------------------------------------
(unless *sys/win32*
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Trim whitespace except current line ----------------------------------------
(defun delete-trailing-whitespace-except-current-line ()
  (interactive)
  (let ((begin (line-beginning-position))
        (end   (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))
(defun smart-delete-trailing-whitespace ()
  (unless (member major-mode '(diff-mode))
    (delete-trailing-whitespace-except-current-line)))
(add-hook 'before-save-hook #'smart-delete-trailing-whitespace)

;; Misc QoL --------------------------------------------------------------------
(delete-selection-mode 1)
(setq x-alt-keysym 'meta)
(save-place-mode 1)
(savehist-mode 1)
(setq history-length 500)

(setq backup-directory-alist
      `(("." . ,(expand-file-name ".backup" user-emacs-directory))))
(setq confirm-kill-emacs     'y-or-n-p
      confirm-kill-processes nil
      ring-bell-function     'ignore
      echo-keystrokes        0.1
      create-lockfiles       nil
      compilation-always-kill t
      compilation-ask-about-save nil
      compilation-scroll-output t
      ad-redefinition-action 'accept)

(setq custom-file (expand-file-name "custom-set-variables.el"
                                    user-emacs-directory))
(load custom-file 'noerror)

(when (fboundp 'global-so-long-mode) (global-so-long-mode 1))
(setq require-final-newline t)
(put 'erase-buffer 'disabled nil)

;; file-type associations
(dolist (pair '(("\\.in\\'"      . text-mode)
                ("\\.out\\'"     . text-mode)
                ("\\.args\\'"    . text-mode)
                ("\\.bb\\'"      . shell-script-mode)
                ("\\.bbclass\\'" . shell-script-mode)
                ("\\.Rmd\\'"     . markdown-mode)))
  (add-to-list 'auto-mode-alist pair))

(add-hook 'after-save-hook #'format-all-buffer)
;; (add-hook 'after-save-hook 'lsp-format-buffer)

(provide '30-editor)
;;; 30-editor.el ends here

;; =========== Font  ===================
(set-face-attribute 'default nil :font "Fira Code Retina" :height 120)
