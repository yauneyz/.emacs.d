;;; "Compiled" snippets and support files for `org-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
		     '(("ts" "#+BEGIN_SRC ts\n$0\n#+END_SRC\n"
			"TS code block" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/cbts"
			nil nil)
		       ("py" "#+BEGIN_SRC python\n$0\n#+END_SRC\n"
			"Python code block" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/cbpy"
			nil nil)
		       ("js" "#+BEGIN_SRC js\n$0\n#+END_SRC\n"
			"JS code block" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/cbjs"
			nil nil)
		       ("go" "#+BEGIN_SRC go\n$0\n#+END_SRC\n"
			"Go code block" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/cbgo"
			nil nil)
		       ("antype" ":ANKI_NOTE_TYPE: ${1:Basic}\n"
			"Anki Note Type" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-type"
			nil nil)
		       ("atag" ":ANKI_TAGS: ${1:tag1 tag2}\n"
			"Anki Tags" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-tags"
			nil nil)
		       ("adeck" ":ANKI_DECK: ${1:Default}\n"
			"Anki Deck" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-deck"
			nil nil)
		       (";ab" "* ${1:Front}\n${2:Back}\n"
			"Anki Basic Card" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-basic"
			nil nil)))


;;; Do not edit! File generated at Wed Nov  5 23:33:50 2025
