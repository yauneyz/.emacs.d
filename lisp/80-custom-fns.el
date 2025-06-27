;;; 80-custom-fns.el --- little helpers & advice  -*- lexical-binding: t; -*-

(defun my-open-init-file ()
  "Open init.el quickly."
  (interactive)
  (find-file user-init-file))

(defun revert-all-buffers ()
  "Revert every non-modified file buffer."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer :ignore-auto :noconfirm))))
  (message "All buffers reverted"))

(defun evil-select-inside-comment-block ()
  "Evil text-object: inside (comment …) block."
  (interactive)
  (when (and (bound-and-true-p evil-mode)
             (eq evil-state 'normal))
    (save-excursion
      (condition-case nil
          (progn
            (while (not (and (looking-at-p "(comment")
                             (eq (char-after (point)) ?\())
                        (not (bobp)))
              (backward-up-list))
            (let ((start (progn (forward-char 8) (point))))
              (forward-sexp)
              (let ((end (1- (point))))
                (evil-visual-select start end))))
        (error (message "Not inside a (comment ...) block"))))))
(evil-define-key 'normal 'global (kbd "<leader>cv") #'evil-select-inside-comment-block)


(defun my-counsel-projectile-find-file ()
  "Use counsel-projectile-find-file with true fuzzy matching.
Temporarily disable ivy-prescient-mode and force ivy--regex-fuzzy
to allow out-of-order matching like 'eve/mp' for 'events/map'."
  (interactive)
  (let ((ivy-prescient-mode nil)
        (ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
    (counsel-projectile-find-file)))

(provide '80-custom-fns)
;;; 80-custom-fns.el ends here
