;;; Compiled snippets and support files for `clojurescript-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'clojurescript-mode
		     '(("<repl" "(comment\n	(let [db @re-frame.db/app-db$1]\n		(println $0)))\n" "reframe-repl" nil nil nil "/home/zac/.emacs.d/snippets/clojurescript-mode/reframe-repl" nil nil)
		       ("rfsub" "(re-frame/subscribe [:$0])\n" "re-frame subscribe" nil nil nil "/home/zac/.emacs.d/snippets/clojurescript-mode/re-frame-sub" nil nil)
		       ("scapi" "[sc.api :refer [defsc spy]]" "import sc api" nil nil nil "/home/zac/.emacs.d/snippets/clojurescript-mode/defsc-spy" nil nil)
		       ("dsc" "(defsc [$0])" "defsc" nil nil nil "/home/zac/.emacs.d/snippets/clojurescript-mode/defsc" nil nil)))


;;; Do not edit! File generated at Wed May 28 11:01:04 2025
