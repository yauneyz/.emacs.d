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
            "  :PROPERTIES:" n
            "  :ANKI_NOTE_TYPE: Basic" n
            "  :END:" n n
            (p "Back") n)

    ";go" '(tempel "#+BEGIN_SRC go" n
		   (p) n
		   "#+END_SRC" n)
    ;; End
    )

  (aas-set-snippets 'go-mode
    ";ptl" '(tempel "fmt.Println(" p ")")
    ";ptf" '(tempel "fmt.Printf(\"" p "\", " p ")")


    )
  )
