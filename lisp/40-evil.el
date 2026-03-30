;;; 40-evil.el --- Evil, which-key, surround …  -*- lexical-binding: t; -*-

;; --------------------------------------------------------------------------
;; Evil core
;; --------------------------------------------------------------------------
;; Note: If yy + p paste behavior is still incorrect after these config changes,
;; ensure you have the latest evil version (bug was fixed in PR #937):
;; Run: M-x straight-pull-package RET evil RET, then restart Emacs
(use-package evil
  :init  (setq evil-want-keybinding nil
               evil-want-C-i-jump  t
               evil-want-integration t)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" #'evil-next-visual-line)
  (evil-global-set-key 'motion "k" #'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode       'normal)
  (evil-set-undo-system 'undo-redo)

  ;; Ensure proper paste behavior for line-wise yanks
  (setq evil-kill-on-visual-paste nil))  ; Don't pollute kill-ring on visual paste

(use-package evil-collection :after evil :config (evil-collection-init))

;; Leader key
(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

;; Clipboard integration: preserve linewise paste behavior ---------------------
(defun my/kill-ring-yank-handler-for-text (text)
  "Return the kill-ring yank-handler for TEXT, if one exists."
  (catch 'handler
    (dolist (entry kill-ring)
      (when (and (stringp entry)
                 (string= entry text))
        (let ((handler (get-text-property 0 'yank-handler entry)))
          (when handler
            (throw 'handler handler)))))
    nil))

;; Restore yank-handler text property when clipboard text already exists in
;; kill-ring. This preserves Evil linewise pastes for text copied in Emacs
;; without forcing unrelated external clipboard text into linewise paste.
(defun my/restore-yank-handler-from-clipboard (orig-fun &rest args)
  "Restore Evil yank metadata for clipboard text copied from Emacs."
  (let* ((text (apply orig-fun args))
         (handler (and (stringp text)
                       (not (get-text-property 0 'yank-handler text))
                       (my/kill-ring-yank-handler-for-text text))))
    (when handler
      (put-text-property 0 (length text) 'yank-handler handler text))
    text))

(advice-add 'current-kill :around #'my/restore-yank-handler-from-clipboard)

(use-package evil-surround :config (global-evil-surround-mode 1))

;; Commenter ------------------------------------------------------------------
(use-package evil-nerd-commenter
  :after evil
  :config
  (evil-define-key '(normal visual) 'global (kbd "<leader>cc")
    #'evilnc-comment-or-uncomment-lines)
  (evil-define-key '(normal visual) 'global (kbd "<leader>cu")
    #'evilnc-comment-or-uncomment-lines))

;; Window move keys -----------------------------------------------------------
(use-package windmove)
(evil-define-key 'normal 'global (kbd "C-h") #'windmove-left)
(evil-define-key 'normal 'global (kbd "C-j") #'windmove-down)
(evil-define-key 'normal 'global (kbd "C-k") #'windmove-up)
(evil-define-key 'normal 'global (kbd "C-l") #'windmove-right)

;; Tab as dispatcher later ----------------------------------------------------
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

(provide '40-evil)
