(defun fountain-mode-setup ()
  (olivetti-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 0)
  (setq fountain-hide-emphasis-markup t)
  (setq fountain-hide-element t)
  (setq fountain-display-scene-numbers-in-margin t))

					; Fountain mode
(use-package fountain-mode
  :hook
  (fountain-mode . fountain-mode-setup)
  :config
  (setq copilot-mode 0)
  (setq company-mode 0)
  ;; Set the way we export, just using screenplain
  ;; To get it to work, just pip install screenplain
  (setq fountain-export-command-profiles '(("screenplain" . "screenplain -f pdf %b ../%B.pdf")))
  (which-function-mode 1))
