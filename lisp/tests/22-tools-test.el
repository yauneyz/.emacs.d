;;; 22-tools-test.el --- Regression tests for treemacs path handling -*- lexical-binding: t; -*-

(require 'ert)
(require 'treemacs)
(require 'treemacs-workspaces)

(ert-deftest my/treemacs-symlinked-paths-match-workspace-projects ()
  "Treemacs should match file paths opened through symlinked parents."
  (let* ((real-root (make-temp-file "treemacs-real-root-" t))
         (link-parent (make-temp-file "treemacs-link-parent-" t))
         (link-root (expand-file-name "repo" link-parent))
         (real-file (expand-file-name "README.md" real-root))
         (link-file (expand-file-name "README.md" link-root))
         test-buffer)
    (unwind-protect
        (progn
          (with-temp-file real-file
            (insert "readme\n"))
          (make-symbolic-link real-root link-root)
          (let* ((project (treemacs-project->create!
                           :name "repo"
                           :path (my/treemacs--canonicalize-existing-path real-root)))
                 (ws (treemacs-workspace->create! :name "WS" :projects (list project)))
                 (treemacs-override-workspace ws))
            (should (string=
                     (my/treemacs--canonicalize-existing-path link-file)
                     (my/treemacs--canonicalize-existing-path real-file)))
            (setq test-buffer (find-file-noselect link-file))
            (with-current-buffer test-buffer
              (should (string= buffer-file-name link-file))
              (should (eq project
                          (my/treemacs--with-canonical-buffer-paths
                           (lambda ()
                             (treemacs-is-path buffer-file-name :in-workspace))))))))
      (when (buffer-live-p test-buffer)
        (kill-buffer test-buffer))
      (when (file-symlink-p link-root)
        (delete-file link-root))
      (when (file-directory-p link-parent)
        (delete-directory link-parent t))
      (when (file-directory-p real-root)
        (delete-directory real-root t)))))
