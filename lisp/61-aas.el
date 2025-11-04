(use-package aas
  :ensure t
  :hook ((prog-mode . aas-activate-for-major-mode)
         (org-mode  . aas-activate-for-major-mode))
  :config
  ;; Helper: call a named Tempel template.
  (defun my/tempel-insert (sym)
    (lambda () (interactive) (tempel-insert sym)))

  ;; --- GLOBAL instant triggers (keep minimal; all should be prefixed) ---
  (aas-set-snippets 'global
    ";->" "→"
    ";shr" "#!/usr/bin/env bash\n")

  (aas-set-snippets 'org-mode
    ";ab" '(tempel
	    "* " (p "Front") n
	    (p "Back") n))
  )
