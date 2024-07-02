;; -*- mode: lisp -*-
;; ~/.spacemacs.d/tempel-templates.eld
;; Commentary
;;
;; Organize this file by mode then by abbreviation.  Multi-modes have their own
;; section

;;; Single Mode Entries

;;;; prog-mode
prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")

;;;; emacs-lisp-mode
emacs-lisp-mode

(autoload ";;;###autoload")
(pt "(point)")
(lambda "(lambda (" p ")" n> r> ")")
(var "(defvar " p "\n  \"" p "\")")
(local "(defvar-local " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(alias "(defalias '" p " '" p ")")
(fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(whenlet "(when-let (" p ")" n> r> ")")
(iflet* "(if-let* (" p ")" n> r> ")")
(whenlet* "(when-let* (" p ")" n> r> ")")
(andlet* "(and-let* (" p ")" n> r> ")")
(cond "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(let "(let (" p ")" n> r> ")")
(let* "(let* (" p ")" n> r> ")")
(rec "(letrec (" p ")" n> r> ")")
(dotimes "(dotimes (" p ")" n> r> ")")
(dolist "(dolist (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(command "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
(advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
        "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(package ";;; " (file-name-base (or (buffer-file-name) (buffer-name))) " --- " p " -*- lexical-binding: t -*-"
         n n
         ";; Copyright (C) " (format-time-string "%Y") " Junghanacs " n
         ";; Author: Junghanacs <junghanacs@gmail.com>" n n
         ";; This file is NOT part of GNU Emacs." n
         ";;; Commentary" n p n
         ";;; Code" n p n
         "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
         ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here" n)

;;;; fundamental-mode
fundamental-mode ;; Available everywhere

(day (format-time-string "%Y-%m-%d"))
(now (format-time-string "%y-%m-%d %-I:%M %p ")) ; 23-02-03 6:58 AM
(nowl (format-time-string "- %y-%m-%d %-I:%M %p :: ")) ; - 23-02-03 6:58 AM ::


;;;; markdown-mode
markdown-mode markdown-ts-mode

(jh "[Junghanacs](https://www.junghanacs.com)")

(header "---
last_modified_at:" (format-time-string " %Y-%m-%d") n
"---" n)

(imagelink "![name](https://raw.githubusercontent.com/junghan0611/graphic-design/main/practicalli-desktop-redux.png)")

;;;; org-mode
org-mode

;; (@ (p (jf/denote-link--people)))
;; ;; Blockquote structure for Emacs
;; (bq "#+attr_shortcode:"
;;     (p "Author (e.g. \"pre\" keyword): " pre t) (unless (s-blank? pre) (concat " :pre " pre))
;;     (p "Cite: " cite t) (unless (s-blank? cite) (concat " :cite " cite))
;;     (p "Cite URL: " cite_url t) (unless (s-blank? cite_url) (concat " :cite_url " cite_url))
;;     n "#+begin_blockquote"
;;     n q
;;     n "#+end_blockquote" n)

;; (cite "{{{cite(" (p (completing-read "Cite: " (jf/org-macro-value-list "cite"))) ")}}}")

(date "[[date:" p "][" p "]]")
(keyboard "{{{kbd(" (p (completing-read "Keyboard: " (jf/org-macro-value-list "kbd"))) ")}}}")
(idiomatic "{{{i(" (p (completing-read "Idiom: " (jf/org-macro-value-list "i"))) ")}}}")
(shell "#+begin_src bash :results scalar replace :exports both :tangle yes" n> q n> "#+end_src" n>)
(rpgs "[[abbr-plural:20221009T115226][RPGs]]")
(rpg "[[abbr:20221009T115226][RPGs]]")
(mk "- [[roam:" (format-time-string "%Y-%m-%d") "]] :: ")
(ii "\n- [[roam:" (format-time-string "%Y-%m-%d") "]] ")
(Today "[[date:" (format-time-string "%Y-%m-%d") "][Today]]")
;; (tor "[[https://takeonrules.com][Take on Rules]]")
(jgi "[[https://junghanacs.github.io][junghanacs.github.io]]")
(uml "#+begin_src plantuml" n> "@startuml" n> "!theme amiga" n> q n> "@enduml" n> "#+end_src" n>)
(localvars "\* Local Variables :noexport:
-----
# Local Variables:
# fill-column: 80
# org-num-max-level: 2
# eval: (auto-fill-mode 1)
# eval: (org-num-mode 1)
# End:" n>)
(project n "#+PROJECT_NAME: " (p "Project Name: " project-name)
         n "#+PROJECT_PATHS: (\"Code, Local\" . \"" p "\")"
         n "#+PROJECT_PATHS: (\"Notes, " (s project-name) "\" . \"" (buffer-file-name)  "\")"
         n "#+PROJECT_PATHS: (\"Project Board\" . \"" p "\")"
         n "#+PROJECT_PATHS: (\"Code, Remote\" . \"" (p "Remote Code: " remote) "\")"
         n "#+PROJECT_PATHS: (\"Actions\" . \"" (s remote) "actions/\")"
         n "#+PROJECT_PATHS: (\"Issues\" . \"" (s remote) "issues/\")"
         n "#+PROJECT_PATHS: (\"Pull Requests\" . \"" (s remote) "pulls/\")")

(update "#+attr_shortcode: :date " (format-time-string "%Y-%m-%d")
        " :mode " (p (completing-read "Mode: " '(inline paragraph sidenote section fallback) nil t) mode)
        n "#+begin_update"
        n q
        n "#+end_update" n)

;; Daily-templates
(daily-templates n "** Awareness 🧘" n "<"
                 (format-time-string "%Y-%m-%d %a 05:30") ">" n
                 "** Deep Work 🎯" n "<"
                 (format-time-string "%Y-%m-%d %a 09:00") ">" n
                 "** Lunch / Brain Wash / Housework" n "<"
                 (format-time-string "%Y-%m-%d %a 12:00") ">" n
                 "** 📖 :reading:" n "<"
                 (format-time-string "%Y-%m-%d %a") ">" n
                 "** Deep Work 🎯" n "<"
                 (format-time-string "%Y-%m-%d %a 14:00") ">" n
                 "** With Family 🧒" n "<"
                 (format-time-string "%Y-%m-%d %a 19:00") ">" n
                 "** Daily Review" n "<"
                 (format-time-string "%Y-%m-%d %a 21:30") ">" n
                 )

;;;; Export Org-roam to Jecklle Markdown

;; (header "
;; #+EXPORT_FILE_NAME: ~/git/blog/_pages/
;; # #+options: broken-links:mark toc:nil author:nil last-modified:nil
;; # #+gfm_tags: markdown zenn
;; #+gfm_custom_front_matter: :last_modified_at 2023-08-15
;; # #+gfm_custom_front_matter: :tags update markdown
;; ")

;; ~/sync/man/dotsamples/vanilla/jousimies-dotfiles/template/tempel
(blog
 "#+DESCRIPTION: " p n>
 "#+DATE: " n>
 "#+AUTHOR: Junganacs" n)

(hugo
 "#+DESCRIPTION: " p n>
 "#+DATE: " p n>
 "#+AUTHOR: Jousimies" p n>
 "#+OPTIONS: author:nil" p n>
 "#+AUTO_EXPORT: t" p n>
 "#+HUGO_BASE_DIR: ~/Documents/hugo-source" n>
 "#+HUGO_AUTO_SET_LASTMOD: t" n>
 "#+HUGO_TAGS:" n>)

(export-html "#+BEGIN_EXPORT HTML" n> r> n> "#+END_EXPORT")
;;;; for org-roam and hugo

;;;; for org-roam and hugo

(hugovideo "#+attr_html: :preload metadata :style center :width 700 :controls t
#+begin_video
<source src=\"/images/" r> "\" type=\"video/mp4\">
Your browser does not support the video tag.
#+end_video")

(header "#+last-modified: Time-stamp: <>
#+glossary_sources:
")


(hugofront "
#+HUGO_SERIES:
#+HUGO_CATEGORIES:
#+EXPORT_FILE_NAME:
#+HUGO_AUTO_SET_LASTMOD: t
#+EXPORT_HUGO_PANDOC_CITATIONS: t
#+cite_export: csl
#+glossary_sources:

#+description: dd
#+options: ':t toc:4 num:t H:8
#+hugo_custom_front_matter: :toc false

# #+hugo_front_matter_key_replace: aliases>nil
# #+hugo_front_matter_key_replace: tags>keywords
# #+print_bibliography:

#+hugo: more

")

;; shortcode hugo
;; (hugoside "#+begin_sidenote" n> r> n> "#+end_sidenote")
;; (hugoinfo "#+attr_shortcode: info" n "#+begin_hint" n> r> n> "#+end_hint")
;; (hugowarning "#+attr_shortcode: warning" n "#+begin_hint" n> r> n> "#+end_hint")
;; (hugodanger "#+attr_shortcode: danger" n "#+begin_hint" n> r> n> "#+end_hint")
;; (hugodetails "#+attr_shortcode: \"Details\"" n "#+begin_details" n> r> n> "#+end_details")

;; link
(lk "[[" p "]]" n )
(zk "
===
#+tags: #테스트 #test
===
<-
===
->
===
")

;;;;; Consider "The One Ring" minor mode to inject these.
;; (rune "{{{rune(" (completing-read "Rune: " jf/gaming/runes) ")}}}")
;; (asl "As {{{i(Loremaster)}}}, ")

;; junghanacs

;; (imagelink "
;; #+attr_html: :width 100% :align center
;; #+caption: noname
;; [[https://raw.githubusercontent.com/junghan0611/graphic-design/main/practicalli-desktop-redux.png]]")
(imglink "#+CAPTION: " p n "#+ATTR_ORG: :width 640px" n "[[file:" p "]]" n)
(imgcaption "#+attr_html: :width 100% :align left" n "#+caption: " p n "#+attr_org: :width 640px" n)

(filelink "[[file:" p "][" p "]]" n )
(filelinkline "[[file:" p "::" p "][" p "]]" n )

;; for slide / presentation
(revealimg "#+REVEAL_HTML: <div class=\"column\" style=\"float; width: 100%\">" n
           "[[file:" p "]]" n
           "#+REVEAL_HTML: </div>" n)

(revealimgleft "#+REVEAL_HTML: <div class=\"column\" style=\"float:left; width: 50%\">" n
               "[[file:" p "]]" n
               "#+REVEAL_HTML: </div>" n)

(revealimgright "#+REVEAL_HTML: <div class=\"column\" style=\"float:right; width: 50%\">" n
                "[[file:" p "]]" n
                "#+REVEAL_HTML: </div>" n)

(title "#+title: " p n "#+author: Junghan Kim" n "#+language: ko" n n)
(description "#+begin_description" n> r> n> "#+end_description")
(details "#+attr_shortcode: \"Details\"
#+begin_details" n> r> n> "#+end_details")
(summary "#+begin_summary" n> r> n> "#+end_summary")
(sidenote "#+begin_sidenote" n> r> n> "#+end_sidenote")

(mark "#+begin_mark" n> r> n> "#+end_mark")
(quote "#+BEGIN_QUOTE" n> r> n> "#+END_QUOTE")
(example "#+BEGIN_EXAMPLE" n> r> n> "#+END_EXAMPLE")
(notes "#+BEGIN_NOTES" n> r> n> "#+END_NOTES") ; reveal.js slides
(center "#+BEGIN_CENTER" n> r> n> "#+END_CENTER")
(comment "#+BEGIN_COMMENT" n> r> n> "#+END_COMMENT")
(mermaid "#+begin_mermaid" n> r> n> "#+end_mermaid")
(ob-mermaid "#+begin_src mermaid exports: code" n> r> n> "#+end_src")
(ob-d2 "#+begin_src d2 exports: code" n> r> n> "#+end_src")

(verse "#+BEGIN_VERSE" n> r> n> "#+END_VERSE")
(bash "#+name:" n> "#+BEGIN_SRC bash :tangle no " n> r> n> "#+END_SRC") ;; :eval no :exports none
(sh "#+BEGIN_SRC shell :tangle no :eval no :exports none" n> r> n> "#+END_SRC")

(hugo-relref "@@hugo:[Title]({{< relref \"docs\" >}})@@")

(python "# #+name:
#+begin_src python :results output" n> r> n> "#+end_src")

(hy "# #+name:
#+begin_src hy :results output" n> r> n> "#+end_src")

(org "#+BEGIN_SRC org" n> r> n> "#+END_SRC")
(elisp "#+BEGIN_SRC elisp" n> r> n> "#+END_SRC")
(elixir "#+BEGIN_SRC elixir" n> r> n> "#+END_SRC")

(gpt "#+BEGIN_SRC chatgpt-shell" n> r> n> "#+END_SRC")

(emacslisp "#+BEGIN_SRC emacs-lisp :tangle yes" n> r> n> "#+END_SRC")
;; (emacslisp "#+BEGIN_SRC emacs-lisp :tangle no :eval no" n> r> n> "#+END_SRC")
(clojure "#+BEGIN_SRC clojure :results code" n> r> n> "#+END_SRC")
;; (clj "#+BEGIN_SRC clojure :results code" n> r> n> "#+END_SRC")
(markdown "#+BEGIN_SRC markdown" n> r> n> "#+END_SRC")
(typescript "#+BEGIN_SRC typescript " n> r> n> "#+END_SRC")
(js "#+BEGIN_SRC javascript " n> r> n> "#+END_SRC")
;; (elispt "#+BEGIN_SRC emacs-lisp :tangle yes" n> r> n> "#+END_SRC") ; tangle yes

(src "#+begin_src" p n> r> n> "#+end_src")
;; (src "#+BEGIN_SRC" p n> r> n> "#+END_SRC" :post (org-edit-src-code))
;; (elisp "#+BEGIN_SRC emacs-lisp" n> r> n "#+END_SRC" :post (org-edit-src-code))

(contact "
:PROPERTIES:
:TYPE:
:GITHUB:
:URL:
:EMAIL:
:NOTE:
:END:")

(bib "
:PROPERTIES:
:TITLE:
:BTYPE: webpage
:AUTHOR:
:URL:
:YEAR: 2023
:CUSTOM_ID:
:END:")

;;;; text-mode
text-mode

;; This is an example of using the value of `(s str)' to populate the above and
;; below line!
(cut "--8<---------------cut here---------------start------------->8---" n r n
     "--8<---------------cut here---------------end--------------->8---" n)
(asciibox "+-" (make-string (length str) ?-) "-+" n
          "| " (s str)                       " |" n
          "+-" (make-string (length str) ?-) "-+" n)
(rot13 (p "plain text" text) n "----" n (rot13 text))
(calc (p "taylor(sin(x),x=0,3)" formula) n "----" n (format "%s" (calc-eval formula)))
(banner comment-start (make-string (length title) ?=) n comment-start (p "Title: " title) n comment-start (make-string (length title) ?=))

;;;; web-mode
web-mode
(bye "<% byebug %>" n)
(db "<% require \"debug\"; binding.break %>" n)
(tor "<cite><a href=\"https://takeonrules.com\">Take on Rules</a></cite>")
(today "<time datetime=\"" (format-time-string "%Y-%m-%d") "\">today</time>")

;;;; eshell-mode
eshell-mode

(for "for " (p "i") " in " p " { " q " }")
(while "while { " p " } { " q " }")
(until "until { " p " } { " q " }")
(if "if { " p " } { " q " }")
(ife "if { " p " } { " p " } { " q " }")
(unl "unless { " p " } { " q " }")
(unle "unless { " p " } { " p " } { " q " }")

;;; Multi-Mode Entries

lisp-mode emacs-lisp-mode ;; Specify multiple modes

(lambda "(lambda (" p ")" n> r> ")")

;;; Clojure and ClojureScript

clojure-mode clojurescript-mode

(-- ";; " (format-time-string "[%Y-%m-%d %H:%M] "))

;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
