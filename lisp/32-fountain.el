;;; 32-fountain.el --- Fountain screenplay editing -*- lexical-binding: t; -*-

(defun fountain-mode-setup ()
  (olivetti-mode 1)
  (visual-line-mode 1)
  (auto-fill-mode 0)
  (setq fountain-hide-emphasis-markup t)
  (setq fountain-hide-element t)
  (setq fountain-display-scene-numbers-in-margin t))

;; --- PDF export

(defun +fountain--pdf-output-file (source-file)
  "Return the PDF output path for SOURCE-FILE."
  (expand-file-name
   (concat (file-name-base source-file) ".pdf")
   (expand-file-name "script_pdf" (file-name-directory source-file))))

(defun +fountain--pdf-file-p (file)
  "Return non-nil when FILE is a nonempty PDF."
  (and (file-regular-p file)
       (> (file-attribute-size (file-attributes file)) 5)
       (with-temp-buffer
         (set-buffer-multibyte nil)
         (insert-file-contents-literally file nil 0 5)
         (string= (buffer-string) "%PDF-"))))

(defun +fountain/export-pdf ()
  "Export the current Fountain file to script_pdf/NAME.pdf with Screenplain.

Create the output directory when needed and replace an existing PDF without
prompting.  Render to a temporary file first so a failed export cannot leave a
truncated PDF behind."
  (interactive)
  (unless buffer-file-name
    (user-error "Save this Fountain buffer before exporting it"))
  (unless (executable-find "screenplain")
    (user-error "screenplain is not installed; rebuild ~/nixos-config first"))
  (save-buffer)
  (let* ((source-file (expand-file-name buffer-file-name))
         (output-file (+fountain--pdf-output-file source-file))
         (output-directory (file-name-directory output-file))
         (output-buffer (get-buffer-create "*Fountain Export*"))
         temporary-file
         exit-code)
    (make-directory output-directory t)
    (setq temporary-file
          (make-temp-file
           (expand-file-name ".fountain-export-" output-directory)
           nil ".pdf"))
    (unwind-protect
        (progn
          (with-current-buffer output-buffer
            (erase-buffer))
          (setq exit-code
                (process-file "screenplain" nil output-buffer nil
                              source-file temporary-file))
          (unless (and (integerp exit-code)
                       (zerop exit-code)
                       (+fountain--pdf-file-p temporary-file))
            (display-buffer output-buffer)
            (user-error "Fountain PDF export failed (exit %s); see %s"
                        exit-code (buffer-name output-buffer)))
          (rename-file temporary-file output-file t)
          (setq temporary-file nil)
          (message "Exported Fountain PDF: %s" output-file)
          output-file)
      (when (and temporary-file (file-exists-p temporary-file))
        (delete-file temporary-file)))))

;; --- Fountain mode

(use-package fountain-mode
  :hook
  (fountain-mode . fountain-mode-setup)
  :bind
  (:map fountain-mode-map
        ("C-c C-e e" . +fountain/export-pdf))
  :config
  (setq company-mode 0)
  (which-function-mode 1))

(provide '32-fountain)
;;; 32-fountain.el ends here
