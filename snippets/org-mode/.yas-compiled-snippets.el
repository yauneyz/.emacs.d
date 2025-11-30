;;; "Compiled" snippets and support files for `org-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
		     '(("zsyntax"
			"#+PROPERTY: ANKI_DECK Current::Z-Syntax::${0}\n"
			"Zac Syntax Deck" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/zsyntax.hm-backup"
			nil nil)
		       ("zsyntax"
			"#+PROPERTY: ANKI_DECK Current::Z-Syntax::${0}\n"
			"Zac Syntax Deck" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/zsyntax"
			nil nil)
		       ("ts" "#+BEGIN_SRC ts\n$0\n#+END_SRC\n"
			"TS code block" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/ts-code.hm-backup"
			nil nil)
		       ("ts" "#+BEGIN_SRC ts\n$0\n#+END_SRC\n"
			"TS code block" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/ts-code"
			nil nil)
		       ("py" "#+BEGIN_SRC python\n$0\n#+END_SRC\n"
			"Python code block" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/py-code.hm-backup"
			nil nil)
		       ("py" "#+BEGIN_SRC python\n$0\n#+END_SRC\n"
			"Python code block" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/py-code"
			nil nil)
		       ("js" "#+BEGIN_SRC js\n$0\n#+END_SRC\n"
			"JS code block" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/js-code.hm-backup"
			nil nil)
		       ("js" "#+BEGIN_SRC js\n$0\n#+END_SRC\n"
			"JS code block" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/js-code"
			nil nil)
		       ("mk" "~${1}~${0}" "Inline code" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/inline-code.hm-backup"
			nil nil)
		       (".."
			(progn
			  (let*
			      ((end (point))
			       (beg
				(save-excursion
				  (skip-chars-backward "^ 	\n")
				  (point))))
			    (if (< beg end)
				(let
				    ((txt
				      (buffer-substring-no-properties
				       beg end)))
				  (delete-region beg end)
				  (insert "~" txt "~"))
			      (insert "~~") (backward-char 1))))
			"Inline code surround" (not (use-region-p))
			nil nil
			"/home/zac/.emacs.d/snippets/org-mode/inline-code-surround.hm-backup"
			nil nil)
		       (".."
			(progn
			  (let*
			      ((end (point))
			       (beg
				(save-excursion
				  (skip-chars-backward "^ 	\n")
				  (point))))
			    (if (< beg end)
				(let
				    ((txt
				      (buffer-substring-no-properties
				       beg end)))
				  (delete-region beg end)
				  (insert "~" txt "~"))
			      (insert "~~") (backward-char 1))))
			"Inline code surround" (not (use-region-p))
			nil nil
			"/home/zac/.emacs.d/snippets/org-mode/inline-code-surround"
			nil nil)
		       ("mk" "~${1}~${0}" "Inline code" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/inline-code"
			nil nil)
		       ("go" "#+BEGIN_SRC go\n$0\n#+END_SRC\n"
			"Go code block" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/go-code.hm-backup"
			nil nil)
		       ("go" "#+BEGIN_SRC go\n$0\n#+END_SRC\n"
			"Go code block" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/go-code"
			nil nil)
		       ("antype" ":ANKI_NOTE_TYPE: ${1:Basic}\n"
			"Anki Note Type" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-type.hm-backup"
			nil nil)
		       ("antype" ":ANKI_NOTE_TYPE: ${1:Basic}\n"
			"Anki Note Type" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-type"
			nil nil)
		       ("atag" ":ANKI_TAGS: ${1:tag1 tag2}\n"
			"Anki Tags" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-tags.hm-backup"
			nil nil)
		       ("atag" ":ANKI_TAGS: ${1:tag1 tag2}\n"
			"Anki Tags" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-tags"
			nil nil)
		       (";ap"
			":PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:END:"
			"Anki Insert Properties" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-properties.hm-backup"
			nil nil)
		       (";ap"
			":PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:END:"
			"Anki Insert Properties" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-properties"
			nil nil)
		       ("adeck" "\\#+PROPERTY: ANKI_DECK ${1}\n"
			"Anki Deck" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-deck.hm-backup"
			nil nil)
		       ("adeck" "\\#+PROPERTY: ANKI_DECK ${1}\n"
			"Anki Deck" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-deck"
			nil nil)
		       (";ago"
			"* ${1:Front}\n  :PROPERTIES:\n  :ANKI_NOTE_TYPE: Basic\n  :END:\n\n#+BEGIN_SRC go\n${2:Back}\n#+END_SRC\n$0"
			"Anki Go Code Card" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-code-go.hm-backup"
			nil nil)
		       (";ago"
			"* ${1:Front}\n  :PROPERTIES:\n  :ANKI_NOTE_TYPE: Basic\n  :END:\n\n#+BEGIN_SRC go\n${2:Back}\n#+END_SRC\n$0"
			"Anki Go Code Card" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-code-go"
			nil nil)
		       (";ab"
			"* ${1:Front}\n  :PROPERTIES:\n  :ANKI_NOTE_TYPE: Basic\n  :END:\n${2:Back}\n"
			"Anki Basic Card" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-basic.hm-backup"
			nil nil)
		       (";ab"
			"* ${1:Front}\n  :PROPERTIES:\n  :ANKI_NOTE_TYPE: Basic\n  :END:\n${2:Back}\n"
			"Anki Basic Card" nil nil nil
			"/home/zac/.emacs.d/snippets/org-mode/anki-basic"
			nil nil)))


;;; Do not edit! File generated at Fri Nov 28 05:11:59 2025
