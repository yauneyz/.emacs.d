;;; init.el --- Load the rest of the configuration -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(let* ((lisp-dir (expand-file-name "lisp" user-emacs-directory))
       (files (directory-files lisp-dir t "\.el$"))) 
  (dolist (file (sort (cl-remove-if-not (lambda (f) (string-match-p "^[0-9]" (file-name-nondirectory f))) files) 'string<))
    (load-file file)))
