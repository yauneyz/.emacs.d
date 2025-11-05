;;; "Compiled" snippets and support files for `snippet-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'snippet-mode
		     '(("new"
			"# -*- mode: snippet -*-\n# name: ${1:snippet-name}\n# key: ${2:trigger}\n# --\n# --\n# mode: `(symbol-name major-mode)`\n${0:snippet-body}"
			"new yasnippet (auto mode)" nil nil nil
			"/home/zac/.emacs.d/snippets/snippet-mode/new"
			nil nil)))


;;; Do not edit! File generated at Wed Nov  5 04:25:39 2025
