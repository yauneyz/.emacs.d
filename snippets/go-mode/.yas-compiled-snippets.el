;;; "Compiled" snippets and support files for `go-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'go-mode
		     '(("tstruct"
			"type ${1:Name} struct {\n	${2:Fields}\n}\n$0\n"
			"Typed struct" nil nil nil
			"/home/zac/.emacs.d/snippets/go-mode/type-struct.hm-backup"
			nil nil)
		       ("tstruct"
			"type ${1:Name} struct {\n	${2:Fields}\n}\n$0\n"
			"Typed struct" nil nil nil
			"/home/zac/.emacs.d/snippets/go-mode/type-struct"
			nil nil)
		       ("scanl" "fmt.Scanln(${0})" "Scan Line" nil nil
			nil
			"/home/zac/.emacs.d/snippets/go-mode/scanln.hm-backup"
			nil nil)
		       ("scanl" "fmt.Scanln(${0})" "Scan Line" nil nil
			nil
			"/home/zac/.emacs.d/snippets/go-mode/scanln"
			nil nil)
		       ("scan" "fmt.Scan(${0})" "Scan" nil nil nil
			"/home/zac/.emacs.d/snippets/go-mode/scan.hm-backup"
			nil nil)
		       ("scan" "fmt.Scan(${0})" "Scan" nil nil nil
			"/home/zac/.emacs.d/snippets/go-mode/scan" nil
			nil)
		       ("ptl" "fmt.Println(${1})$0" "println" nil nil
			nil
			"/home/zac/.emacs.d/snippets/go-mode/println.hm-backup"
			nil nil)
		       ("ptl" "fmt.Println(${1})$0" "println" nil nil
			nil
			"/home/zac/.emacs.d/snippets/go-mode/println"
			nil nil)
		       ("ptf" "fmt.Printf(\"${1}\", ${2})$0" "printf"
			nil nil nil
			"/home/zac/.emacs.d/snippets/go-mode/printf.hm-backup"
			nil nil)
		       ("ptf" "fmt.Printf(\"${1}\", ${2})$0" "printf"
			nil nil nil
			"/home/zac/.emacs.d/snippets/go-mode/printf"
			nil nil)
		       ("pt" "fmt.Print(\"${1}\")$0" "print" nil nil
			nil
			"/home/zac/.emacs.d/snippets/go-mode/print.hm-backup"
			nil nil)
		       ("pt" "fmt.Print(\"${1}\")$0" "print" nil nil
			nil
			"/home/zac/.emacs.d/snippets/go-mode/print"
			nil nil)
		       ("importf"
			"import (\n       \"fmt\"${1}\n)\n${0}\n"
			"Import fmt" nil nil nil
			"/home/zac/.emacs.d/snippets/go-mode/importf.hm-backup"
			nil nil)
		       ("importf"
			"import (\n       \"fmt\"${1}\n)\n${0}\n"
			"Import fmt" nil nil nil
			"/home/zac/.emacs.d/snippets/go-mode/importf"
			nil nil)
		       ("import" "import (\n	\"${1}\"${0}\n)\n"
			"Grouped Import" nil nil nil
			"/home/zac/.emacs.d/snippets/go-mode/import-grouped.hm-backup"
			nil nil)
		       ("import" "import (\n	\"${1}\"${0}\n)\n"
			"Grouped Import" nil nil nil
			"/home/zac/.emacs.d/snippets/go-mode/import-grouped"
			nil nil)))


;;; Do not edit! File generated at Fri Nov 28 05:11:59 2025
