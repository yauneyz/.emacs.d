;;; "Compiled" snippets and support files for `go-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'go-mode
		     '(("type-struct" "" "type-struct" nil nil nil
			"/home/zac/.emacs.d/snippets/go-mode/type-struct"
			nil nil)
		       ("tstruct"
			"type ${1:Name} struct {\n	${2:Fields}\n}\n$0\n"
			"Typed struct" nil nil nil
			"/home/zac/.emacs.d/snippets/go-mode/tsruct"
			nil nil)
		       ("import" "import (\n	\"${1}\"${0}\n)\n"
			"Grouped Import" nil nil nil
			"/home/zac/.emacs.d/snippets/go-mode/import-grouped"
			nil nil)))


;;; Do not edit! File generated at Wed Nov  5 04:25:39 2025
