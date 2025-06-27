;;; 40-evil.el --- Evil, which-key, surround …  -*- lexical-binding: t; -*-

;; --------------------------------------------------------------------------
;; Evil core
;; --------------------------------------------------------------------------
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
  (evil-set-undo-system 'undo-redo))

(use-package evil-collection :after evil :config (evil-collection-init))

;; Leader key
(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

(use-package evil-surround :config (global-evil-surround-mode 1))

;; Commenter ------------------------------------------------------------------
(install-if-necessary 'evil-nerd-commenter)
(evil-define-key '(normal visual) 'global (kbd "<leader>cc")
  #'evilnc-comment-or-uncomment-lines)
(evil-define-key '(normal visual) 'global (kbd "<leader>cu")
  #'evilnc-comment-or-uncomment-lines)


;; Window move keys -----------------------------------------------------------
(use-package windmove)
(evil-define-key 'normal 'global (kbd "C-h") #'windmove-left)
(evil-define-key 'normal 'global (kbd "C-j") #'windmove-down)
(evil-define-key 'normal 'global (kbd "C-k") #'windmove-up)
(evil-define-key 'normal 'global (kbd "C-l") #'windmove-right)

;; Tab as dispatcher later ----------------------------------------------------
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)


(install-if-necessary 'evil-nerd-commenter)
;; (evilnc-default-hotkeys)
(evil-define-key '(normal visual) 'global (kbd "<leader>cc") 'evilnc-comment-or-uncomment-lines)
(evil-define-key '(normal visual) 'global (kbd "<leader>cu") 'evilnc-comment-or-uncomment-lines)

(provide '40-evil)
