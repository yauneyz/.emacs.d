
;; Map Alt key to Meta
(setq x-alt-keysym 'meta)

;; Debug on error
;; (setq debug-on-error t)

;;; When buffer is closed, saves the cursor location
					;(save-place-mode 1)

;; Set history-length longer
(setq-default history-length 500)

(setq tab-width 4)

;; Default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "firefox")

;; Move the backup fies to user-emacs-directory/.backup
(setq backup-directory-alist `(("." . ,(expand-file-name ".backup" user-emacs-directory))))

;; Ask before killing emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Automatically kill all active processes when closing Emacs
(setq confirm-kill-processes nil)

;; Turn Off Cursor Alarms
(setq ring-bell-function 'ignore)

;; Show Keystrokes in Progress Instantly
(setq echo-keystrokes 0.1)

;; Don't Lock Files
(setq-default create-lockfiles nil)

;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another

(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'

(setq-default compilation-scroll-output t)

;; ad-handle-definition warnings are generated when functions are redefined with `defadvice',
;; they are not helpful.
(setq ad-redefinition-action 'accept)

;; Move Custom-Set-Variables to Different File
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
					;(when (fboundp 'global-so-long-mode)
					;(global-so-long-mode))

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Enable `erase-buffer' function
(put 'erase-buffer 'disabled nil)

;; Default .args, .in, .out files to text-mode
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.bb\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.bbclass\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))


(setq history-length 25)
					;(savehist-mode 1)


(setenv "XDG_CACHE_HOME" (expand-file-name "~/.cache"))
