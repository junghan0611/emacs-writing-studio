;;; init.el --- Emacs Writing Studio init -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Peter Prevos

;; Author: Peter Prevos <peter@prevos.net>
;; Maintainer: Peter Prevos <peter@prevos.net>

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Emacs Writing Studio init file
;; https://lucidmanager.org/tags/emacs
;;
;; This init file is tangled from the Org mode source:
;; documents/ews-book/99-appendix.org
;;
;;; Code:

;; Emacs 29? EWS leverages functionality from the latest Emacs version.

(when (< emacs-major-version 29)
  (error "Emacs Writing Studio requires Emacs version 29 or later"))

;; Custom settings in a separate file and load the custom settings

(setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))

(keymap-global-set "C-c w v" 'customize-variable)

;; Set package archives

(use-package package
  :config
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (package-initialize)
  )

;; Package Management

(use-package use-package
  :custom
  (use-package-always-ensure t)
  (package-native-compile t)
  ;; (warning-minimum-level :emergency)
  )

;; Load EWS functions

(load-file (concat (file-name-as-directory user-emacs-directory) "ews.el"))

;; Check for missing external software
;;
;; - soffice (LibreOffice): View and create office documents
;; - zip: Unpack ePub documents
;; - pdftotext (poppler-utils): Convert PDF to text
;; - djvu (DjVuLibre): View DjVu files
;; - curl: Reading RSS feeds
;; - divpng: Part of LaTeX
;; - dot (GraphViz): Create note network diagrams
;; - convert (ImageMagick): Convert image files 
;; - gm (GraphicsMagick): Convert image files
;; - latex (TexLive, MacTex or MikTeX): Preview LaTex and export Org to PDF
;; - hunspell: Spellcheck. Also requires a hunspell dictionary
;; - grep: Search inside files
;; - ripgrep: Faster alternative for grep
;; - gs (GhostScript): View PDF files
;; - mutool (MuPDF): View PDF files
;; - mpg321, ogg123 (vorbis-tools), mplayer, mpv, vlc: Media players
;; - git: Version control

(ews-missing-executables
 '("soffice" "zip" "pdftotext" ;; "ddjvu"
   "curl"
   "dvipng"
   "dot"
   ("convert" "gm")
   "latex"
   "hunspell"
   ("grep" "ripgrep")
   ("gs" "mutool")
   ("mpg321" "ogg123" "mplayer" "mpv" "vlc")
   "git"))

;;; LOOK AND FEEL
;; Keyboard-centric user interface removing tool, menu and scroll bars

(tool-bar-mode -1)
(menu-bar-mode -1)
;; (scroll-bar-mode -1)

;; Short answers only please

(setq use-short-answers t)

;; Spacious padding

(use-package spacious-padding
  :if window-system
  :custom
  (line-spacing 3)
  :hook (server-after-make-frame . spacious-padding-mode)
  :init
  (setq spacious-padding-widths
        '(:internal-border-width 15 ; 15
                                 :header-line-width 4
                                 :mode-line-width 4 ; 6
                                 :tab-width 4
                                 :right-divider-width 15 ; 30
                                 :scroll-bar-width 8
                                 :fringe-width 10)) ; 8
  :config
  (spacious-padding-mode 1))

;; Modus Themes

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  ;; (modus-themes-mixed-fonts t)
  (modus-themes-to-toggle
   '(modus-operandi modus-vivendi-tinted))
  ;; :init
  ;; (load-theme 'modus-operandi :no-confirm)
  :bind
  (("C-c w t t" . modus-themes-toggle)
   ("C-c w t m" . modus-themes-select)
   ("C-c w t s" . consult-theme)))

;; (use-package mixed-pitch
;;   :hook
;;   (text-mode . mixed-pitch-mode))

;; Window management
;; Split windows sensibly

(setq split-width-threshold 120
      split-height-threshold nil)

;; Keep window sizes balanced

(use-package balanced-windows
  :config
  (balanced-windows-mode))

;; MINIBUFFER COMPLETION

;; Enable vertico

(use-package vertico
  :init
  (vertico-mode)
  :custom
  (vertico-sort-function 'vertico-sort-history-alpha))

;; Persist history over Emacs restarts.

(use-package savehist
  :init
  (savehist-mode))

;; Search for partial matches in any order

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides
   '((file (styles partial-completion)))))

;; Enable richer annotations using the Marginalia package

(use-package marginalia
  :init
  (marginalia-mode))

;; Improve keyboard shortcut discoverability

(use-package which-key
  :config
  (which-key-mode)
  :custom
  (which-key-max-description-length 40)
  (which-key-lighter nil)
  (which-key-sort-order 'which-key-description-order))

;; Improved help buffers

(use-package helpful
  :bind
  (("C-h f" . helpful-function)
   ("C-h x" . helpful-command)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable)))

;;; Text mode settings

(use-package text-mode
  :ensure
  nil
  :hook
  (text-mode . visual-line-mode)
  :init
  (delete-selection-mode t)
  :custom
  (sentence-end-double-space nil)
  (scroll-error-top-bottom t)
  (save-interprogram-paste-before-kill t))

;; Check spelling with flyspell and hunspell

(use-package flyspell
  :custom
  (ispell-program-name "hunspell")
  (ispell-dictionary ews-hunspell-dictionaries)
  (flyspell-mark-duplications-flag nil) ;; Writegood mode does this
  (org-fold-core-style 'overlays) ;; Fix Org mode bug
  :config
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic ews-hunspell-dictionaries)
  :hook
  (text-mode . flyspell-mode)
  :bind
  (("C-c w s s" . ispell)
   ("C-;"       . flyspell-auto-correct-previous-word)))

;;; Ricing Org mode

(use-package org
  :custom
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(450))
  (org-fold-catch-invisible-edits 'error)
  (org-startup-with-latex-preview t)
  (org-pretty-entities t)
  (org-use-sub-superscripts "{}")
  (org-id-link-to-org-use-id t))

;; Show hidden emphasis markers

(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

;; LaTeX previews

(use-package org-fragtog
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

;; Org modern: Most features disables for beginnng users

;; (use-package org-modern
;;   :hook
;;   (org-mode . org-modern-mode)
;;   :custom
;;   (org-modern-table nil)
;;   (org-modern-keyword nil)
;;   (org-modern-timestamp nil)
;;   (org-modern-priority nil)
;;   (org-modern-checkbox nil)
;;   (org-modern-tag nil)
;;   (org-modern-block-name nil)
;;   (org-modern-keyword nil)
;;   (org-modern-footnote nil)
;;   (org-modern-internal-target nil)
;;   (org-modern-radio-target nil)
;;   (org-modern-statistics nil)
;;   (org-modern-progress nil))

;; INSPIRATION

;; Doc-View

(use-package doc-view
  :custom
  (doc-view-resolution 300)
  (large-file-warning-threshold (* 50 (expt 2 20))))

;; Read ePub files

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Reading LibreOffice files
;; Fixing a bug in Org Mode pre 9.7
;; Org mode clobbers associations with office documents

(use-package ox-odt
  :ensure nil
  :config
  (add-to-list 'auto-mode-alist
               '("\\.\\(?:OD[CFIGPST]\\|od[cfigpst]\\)\\'"
                 . doc-view-mode-maybe)))

;; Managing Bibliographies

(use-package bibtex
  :custom
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file"     "Relative or absolute path to attachments" "" )))
  (bibtex-align-at-equal-sign t)
  :config
  (ews-bibtex-register)
  :bind
  (("C-c w b r" . ews-bibtex-register)))

;; Biblio package for adding BibTeX records

(use-package biblio
  :bind
  (("C-c w b b" . ews-bibtex-biblio-lookup)))

;; Citar to access bibliographies

(use-package citar
  :defer t
  :custom
  (citar-bibliography ews-bibtex-files)
  :bind
  (("C-c w b o" . citar-open)))

(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode)
  :bind (("C-M-." . embark-act)
	 :map citar-embark-citation-map
	 ("c" . citar-denote-find-citation)))

;; Read RSS feeds with Elfeed

(use-package elfeed
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer)
  :bind
  ("C-c w e" . elfeed))

;; Configure Elfeed with org mode

(use-package elfeed-org
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files
   (list (concat (file-name-as-directory
		  (getenv "HOME"))
                 "Documents/elfeed.org"))))

;; Easy insertion of weblinks

(use-package org-web-tools
  :bind
  (("C-c w w" . org-web-tools-insert-link-for-url)))

;; Emacs Multimedia System

(use-package emms
  :if window-system
  :init
  (require 'emms-setup)
  (require 'emms-mpris)
  (emms-all)
  (emms-default-players)
  (emms-mpris-enable)
  :custom
  (emms-browser-covers #'emms-browser-cache-thumbnail-async)
  :bind
  (("C-c w m b" . emms-browser)
   ("C-c w m e" . emms)
   ("C-c w m p" . emms-play-playlist )
   ("<XF86AudioPrev>" . emms-previous)
   ("<XF86AudioNext>" . emms-next)
   ("<XF86AudioPlay>" . emms-pause)))

(use-package openwith
  :config
  (openwith-mode t)
  :custom
  (openwith-association nil))

;; Fleeting notes

(use-package org
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)))

;; Capture templates

(setq org-capture-templates
      '(("f" "Fleeting note"
	 item
	 (file+headline org-default-notes-file "Notes")
	 "- %?")
	("p" "Permanent note" plain
	 (file denote-last-path)
	 #'denote-org-capture
	 :no-save t
	 :immediate-finish nil
	 :kill-buffer t
	 :jump-to-captured t)
	("t" "New task" entry
	 (file+headline org-default-notes-file "Tasks")
	 "* TODO %i%?")))

;; Denote

(use-package denote
  :defer t
  :custom
  (denote-sort-keywords t)
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :init
  (require 'denote-org-extras)
  :bind
  (("C-c w d b" . denote-find-backlink)
   ("C-c w d d" . denote-date)
   ("C-c w d f" . denote-find-link)
   ("C-c w d h" . denote-org-extras-link-to-heading)
   ("C-c w d i" . denote-link-or-create)
   ("C-c w d I" . denote-org-extras-dblock-insert-links)
   ("C-c w d k" . denote-keywords-add)
   ("C-c w d K" . denote-keywords-remove)
   ("C-c w d l" . denote-link-find-file)
   ("C-c w d n" . denote)
   ("C-c w d r" . denote-rename-file)
   ("C-c w d R" . denote-rename-file-using-front-matter)))

;; Consult-Denote for easy access

(use-package consult-denote
  :custom
  (consult-denote-find-command
   #'(lambda() (find-file (consult-denote-file-prompt))))
  :config
  (consult-denote-mode)
  :bind
  (("C-c w h" . consult-org-heading)
   ("C-c w f" . consult-denote-find)
   ("C-c w g" . consult-denote-grep)
   ("C-x b"   . consult-buffer)))

;; Citar-Denote to manage literature notes

(use-package citar-denote
  :custom
  (citar-open-always-create-notes t)
  :init
  (citar-denote-mode)
  :bind
  (("C-c w b c" . citar-create-note)
   ("C-c w b n" . citar-denote-open-note)
   ("C-c w b x" . citar-denote-nocite)
   :map org-mode-map
   ("C-c w b k" . citar-denote-add-citekey)
   ("C-c w b K" . citar-denote-remove-citekey)
   ("C-c w b d" . citar-denote-dwim)
   ("C-c w b e" . citar-denote-open-reference-entry)))

;; Explore and manage your Denote collection

(use-package denote-explore
  :bind
  (;; Statistics
   ("C-c w x c" . denote-explore-count-notes)
   ("C-c w x C" . denote-explore-count-keywords)
   ("C-c w x b" . denote-explore-keywords-barchart)
   ("C-c w x x" . denote-explore-extensions-barchart)
   ;; Random walks
   ("C-c w x r" . denote-explore-random-note)
   ("C-c w x l" . denote-explore-random-link)
   ("C-c w x k" . denote-explore-random-keyword)
   ;; Denote Janitor
   ("C-c w x d" . denote-explore-identify-duplicate-notes)
   ("C-c w x z" . denote-explore-zero-keywords)
   ("C-c w x s" . denote-explore-single-keywords)
   ("C-c w x o" . denote-explore-sort-keywords)
   ("C-c w x w" . denote-explore-rename-keyword)
   ;; Visualise denote
   ("C-c w x n" . denote-explore-network)
   ("C-c w x v" . denote-explore-network-regenerate)
   ("C-c w x D" . denote-explore-degree-barchart)))

;; Set some Org mode shortcuts

(use-package org
  :bind
  (:map org-mode-map
        ("C-c w n" . ews-org-insert-notes-drawer)
        ("C-c w p" . ews-org-insert-screenshot)
        ("C-c w c" . ews-org-count-words)))

;; Distraction-free writing

(use-package olivetti
  :demand t
  :bind
  (("C-c w o" . ews-olivetti)))

;; Undo Tree

;; (use-package undo-tree
;;   :config
;;   (global-undo-tree-mode)
;;   :custom
;;   (undo-tree-auto-save-history nil)
;;   :bind
;;   (("C-c w u" . undo-tree-visualize)))

;; Export citations with Org Mode

(require 'oc-natbib)
(require 'oc-csl)

(setq org-cite-csl-styles-dir ews-bibtex-directory
      org-cite-export-processors
      '((latex natbib "apalike2" "authoryear")
        (t     csl    "apa6.csl"))
      org-cite-global-bibliography ews-bibtex-files
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)

;; Lookup words in online dictionary

(use-package dictionary
  :custom
  (dictionary-server "dict.org")
  :bind
  (("C-c w s d" . dictionary-lookup-definition)))

(use-package powerthesaurus
  :bind
  (("C-c w s p" . powerthesaurus-transient)))

;; Writegood-Mode for buzzwords, passive writing and repeated word detection

(use-package writegood-mode
  :bind
  (("C-c w s r" . writegood-reading-ease))
  :hook
  (text-mode . writegood-mode))

;; Abbreviations

(add-hook 'text-mode-hook 'abbrev-mode)

;; Lorem Ipsum generator

(use-package lorem-ipsum
  :custom
  (lorem-ipsum-list-bullet "- ") ;; Org mode bullets
  :init
  (setq lorem-ipsum-sentence-separator (if sentence-end-double-space "  " " "))
  :bind
  (("C-c w i s" . lorem-ipsum-insert-sentences)
   ("C-c w i p" . lorem-ipsum-insert-paragraphs)
   ("C-c w i l" . lorem-ipsum-insert-list)))

;; ediff

(use-package ediff
  :ensure nil
  :custom
  (ediff-keep-variants nil)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package fountain-mode)

(use-package markdown-mode)

;; Generic Org Export Settings

(use-package org
  :custom
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-broken-links t)
  (org-export-with-toc nil)
  (org-export-with-smart-quotes t)
  (org-export-date-timestamp-format "%e %B %Y"))

;; LaTeX PDF Export settings

(use-package ox-latex
  :ensure nil
  :demand t
  :custom
  ;; Multiple LaTeX passes for bibliographies
  (org-latex-pdf-process
   '("pdflatex -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; Clean temporary files after export
  (org-latex-logfiles-extensions
   (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
           "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
           "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
           "tex" "bcf"))))

;; LaTeX templates

(with-eval-after-load 'ox-latex
  (add-to-list
   'org-latex-classes
   '("crc"
     "\\documentclass[krantz2]{krantz}
        \\usepackage{lmodern}
        \\usepackage[authoryear]{natbib}
        \\usepackage{nicefrac}
        \\usepackage[bf,singlelinecheck=off]{caption}
        \\captionsetup[table]{labelsep=space}
        \\captionsetup[figure]{labelsep=space}
        \\usepackage{Alegreya}
        \\usepackage[scale=.8]{sourcecodepro}
        \\usepackage[breaklines=true]{minted}
        \\usepackage{rotating}
        \\usepackage[notbib, nottoc,notlot,notlof]{tocbibind}
        \\usepackage{amsfonts, tikz, tikz-layers}
        \\usetikzlibrary{fadings, quotes, shapes, calc, decorations.markings}
        \\usetikzlibrary{patterns, shadows.blur}
        \\usetikzlibrary{shapes,shapes.geometric,positioning}
        \\usetikzlibrary{arrows, arrows.meta, backgrounds}
        \\usepackage{imakeidx} \\makeindex[intoc]
        \\renewcommand{\\textfraction}{0.05}
        \\renewcommand{\\topfraction}{0.8}
        \\renewcommand{\\bottomfraction}{0.8}
        \\renewcommand{\\floatpagefraction}{0.75}
        \\renewcommand{\\eqref}[1]{(Equation \\ref{#1})}
        \\renewcommand{\\LaTeX}{LaTeX}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\paragraph*{%s}"))))

(use-package ox-epub
  :demand t)

;; ADVANCED NDOCUMENTED EXPORT SETTINGS FOR EWS

;; Use GraphViz for flow diagrams
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t))) ; this line activates dot

;;; ADMINISTRATION

;; Bind org agenda command

(use-package org
  :custom
  (org-log-into-drawer t)
  (org-enforce-todo-checkbox-dependencies t)
  (org-enforce-todo-dependencies t)
  :bind
  (("C-c a" . org-agenda)))

;; FILE MANAGEMENT

(use-package dired
  :ensure
  nil
  :commands
  (dired dired-jump)
  :custom
  (dired-listing-switches
   "-goah --group-directories-first --time-style=long-iso")
  (dired-dwim-target t)
  (delete-by-moving-to-trash t)
  :init
  (put 'dired-find-alternate-file 'disabled nil))

;; Hide hidden files

(use-package dired-hide-dotfiles
  :hook
  (dired-mode . dired-hide-dotfiles-mode)
  :bind
  (:map dired-mode-map ("." . dired-hide-dotfiles-mode)))

;; Backup files

(setq-default backup-directory-alist
              `(("." . ,(expand-file-name "backups/" user-emacs-directory)))
              version-control t
              delete-old-versions t
              create-lockfiles nil)

;; Recent files

(use-package recentf
  :config
  (recentf-mode t)
  (run-at-time nil (* 5 60)
               (lambda () (let ((save-silently t))
                            (recentf-save-list))))
  :custom
  (recentf-max-saved-items 50)
  :bind
  (("C-c w r" . recentf-open)))

;; Bookmarks

(use-package bookmark
  :custom
  (bookmark-save-flag 1)
  :bind
  ("C-x r D" . bookmark-delete))


;;; USER-CONFIGURATION

;;;; Load 'Per-Machine'

;; Most of my per-environment config done via =customize= and is in .custom.el.
;; However, some config is more involved, such as packages I just want in one
;; environment and not the others.  To that end, let's load a file that can contain
;; those customizations.
(let ((per-machine-filename (concat user-emacs-directory "per-machine.el")))
  (when (file-exists-p per-machine-filename)
    (load-file per-machine-filename)))

;;;; Overide path and configs

(setq ews-bibtex-directory (concat user-org-directory "bib"))
(setq ews-hunspell-dictionaries "ko_KR")
(setq rmh-elfeed-org-files (concat user-org-directory "elfeed.org"))
(setq denote-directory user-org-directory)

(setq citar-bibliography config-bibfiles)
;; use #+cite_export: csl apa.csl
(setq org-cite-csl-styles-dir (concat user-org-directory ".csl"))
(setq citar-citeproc-csl-styles-dir (concat user-org-directory ".csl"))
;; (setq citar-citeproc-csl-locales-dir "~/.csl/locales")
(setq citar-citeproc-csl-style "apa.csl") ; ieee.csl
(setq citar-notes-paths '("~/sync/org/bib/"))
(setq org-cite-global-bibliography config-bibfiles)

;;;; Load Evil

(load-file (concat (file-name-as-directory user-emacs-directory) "evil.el"))

;;;; basics

;; (setq-default display-line-numbers-width-start t) ; doom's default t
(setq inhibit-compacting-font-caches t)
(setq inhibit-startup-screen t)

;; Stop asking abount following symlinks to version controlled files
(setq vc-follow-symlinks t)

;; default 120 emacs-29, 60 emacs-28
(setq kill-ring-max 30) ; keep it small

;; Disable .# lock files
(setq create-lockfiles nil)

;; Ridiculous path view is vanilla emacs. change truename!
;; truename 을 원하지 않는다. 심볼링링크대로 쓰고 싶다. nil 로 사용한다.
(setq find-file-visit-truename t)

;; Show recursion depth in minibuffer (see `enable-recursive-minibuffers')
(minibuffer-depth-indicate-mode 1) ;; default nil

;; Shr group: Simple HTML Renderer 를 의미한다. 여기 설정을 바꾸면 faces 를 수정할 수 있음
(setq shr-use-fonts nil)

;; http://yummymelon.com/devnull/surprise-and-emacs-defaults.html
;;텍스트를 선택한 다음 그 위에 입력하면 해당 텍스트가 삭제되어야 합니다.
;;놀랍게도 기본 Emac 에서는 이 동작이 기본적으로 제공되지 않습니다. 명시적으로
;;활성화해야 합니다.
(setq delete-selection-mode t) ; default nil
;; (setq magit-save-repository-buffers 'dontask) ; default t

;; Show a message when garbage collection happens? Useful while tuning the GC
;; (setq garbage-collection-messages t)

;; (setq ring-bell-function 'ignore)

;;;; tab-width

;; ====== Buffer-local variables ======
(setq-default
 ;; Display long lines
 truncate-lines nil ; default t
 ;; Default fill column width
 fill-column 80
 ;; Never mix, use only spaces
 indent-tabs-mode nil ;; Width for line numbers display-line-numbers-width 4

 ;; 1) per major-mode config or hook
 ;; 2) editorconfig
 ;; 3) tab-width 4 (below)
 ;; tab-width 4 ;; 2024-03-11 org-mode element-cache 사용 시 무조건 8이다. 충돌난다. 끈다.

 display-line-numbers-width-start t ; 2024-06-26
 )

;;;; display-Line-Numbers-Mode

(setq display-line-numbers-type 'relative)

;; (dolist (mode '(term-mode-hook
;;                 shell-mode-hook
;;                 eshell-mode-hook
;;                 vterm-mode-hook))
;;   (add-hook mode #'my/disable-line-numbers))

;; (add-hook 'org-mode-hook 'display-line-numbers-mode)
;; (add-hook 'markdown-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;;;; time

(require 'time)
(setq display-time-format " | %a %e %b, %H:%M | ")
;; Covered by `display-time-format'
;; (setq display-time-24hr-format t)
;; (setq display-time-day-and-date t)
(setq display-time-interval 30) ; default 60
(setq display-time-default-load-average nil)

;; NOTE 2022-09-21: For all those, I have implemented my own solution
;; that also shows the number of new items, although it depends on
;; notmuch: the `notmuch-indicator' package.
(setq display-time-mail-directory nil)
(setq display-time-mail-function nil)
(setq display-time-use-mail-icon nil)
(setq display-time-mail-string nil)
(setq display-time-mail-face nil)

;; World clock
(setq zoneinfo-style-world-list
      '(("America/Los_Angeles" "Los Angeles")
        ("America/Chicago" "Chicago")
        ("Brazil/Acre" "Rio Branco")
        ("America/New_York" "New York")
        ("Brazil/East" "Brasília")
        ("Europe/Lisbon" "Lisbon")
        ("Europe/Brussels" "Brussels")
        ("Europe/Athens" "Athens")
        ("Asia/Tbilisi" "Tbilisi")
        ("Asia/Yekaterinburg" "Yekaterinburg")
        ("Asia/Shanghai" "Shanghai")
        ("Asia/Seoul" "Seoul")
        ("Asia/Vladivostok" "Vladivostok")))

;; All of the following variables are for Emacs 28
(setq world-clock-list t)
(setq world-clock-time-format "%R %z  %A %d %B")
(setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
(setq world-clock-timer-enable t)
(setq world-clock-timer-second 60)

;;;; calendar

(require 'calendar)
;; (setq org-agenda-start-on-weekday nil)
(add-hook 'calendar-today-visible-hook 'calendar-mark-today)
(setq calendar-date-style 'iso ;; YYYY/MM/DD
      calendar-mark-holidays-flag t
      calendar-week-start-day 1 ;; 0 Sunday, 1 Monday
      calendar-mark-diary-entries-flag nil
      calendar-latitude user-calendar-latitude
      calendar-longitude user-calendar-longitude
      calendar-location-name user-calendar-location-name
      calendar-time-display-form
      '(24-hours ":" minutes
        (if time-zone " (") time-zone (if time-zone ")")))

;;;; completion

(use-package consult
  :commands consult-ripgrep-noignore
  :init
  (advice-add #'project-find-regexp :override #'consult-ripgrep)
  (advice-add #'project-switch-to-buffer :override #'consult-project-buffer)
  :config
  (setq consult-narrow-key "?"
        consult-preview-key "M-.")

  (defun consult-delete-default-contents ()
    (remove-hook 'pre-command-hook 'consult-delete-default-contents)
    (cond ((member this-command '(self-insert-command))
           (delete-minibuffer-contents))
          (t (put-text-property (minibuffer-prompt-end) (point-max) 'face 'default))))

  (consult-customize consult-theme
                     :preview-key '(:debounce 0.2 any)
                     consult-goto-line consult-imenu consult-line
                     :preview-key 'any
                     consult-line
                     :initial (when-let ((string (thing-at-point 'word)))
                                (add-hook 'pre-command-hook 'consult-delete-default-contents)
                                (propertize string 'face 'shadow)))

  ;; https://github.com/minad/consult/wiki#temporarily-override-consult-ripgrep-args
  (defun consult--ripgrep-noignore-builder (input)
    "consult--ripgrep-builder with INPUT, but ignores .gitignore."
    (let ((consult-ripgrep-args
           (if (string-match-p "--no-ignore-vcs" consult-ripgrep-args)
               consult-ripgrep-args
             (concat consult-ripgrep-args "--no-ignore-vcs ."))))
      (consult--make-ripgrep-builder input)))

  (defun consult-ripgrep-noignore (&optional dir initial)
    "Do consult-ripgrep with DIR and INITIAL, but without ignoring."
    (interactive "P")
    (consult--grep "Ripgrep"
                   #'consult--ripgrep-noignore-builder
                   ;; Here the directory prompt is called by default to avoid searching from the project root
                   (if dir dir t) initial))

  (defvar consult--source-project-file
    `(:name     "Project File"
      :narrow   ?f
      :category file
      :face     consult-file
      :history  file-name-history
      :state    ,#'consult--file-state
      :enabled  ,(lambda () consult-project-function)
      :items
      ,(lambda ()
         (when-let (project (project-current t))
           (let* ((all-files (project-files project))
                  (common-parent-directory
                   (let ((common-prefix (try-completion "" all-files)))
                     (if (> (length common-prefix) 0)
                         (file-name-directory common-prefix))))
                  (cpd-length (length common-parent-directory))
                  items)
             (print all-files)
             (dolist (file all-files items)
               (let ((part (substring file cpd-length)))
                 (when (equal part "") (setq part "./"))
                 (put-text-property 0 1 'multi-category `(file . ,file) part)
                 (push part items))))))
      "Project file candidate source for `consult-buffer'."))

  (defvar consult--source-project-file-hidden
    `(:hidden t :narrow (?f . "Project File") ,@consult--source-project-file)
    "Like `consult--source-project-file' but hidden by default.")

  (defvar consult--source-project-recent-file-override
    `(:name "Recent File" :narrow (?r . "Recent File") ,@consult--source-project-file)
    "Like `consult--source-recent-file' but overridden the narrow key.")

  (setq consult-project-buffer-sources
        '(consult--source-project-buffer
          consult--source-project-recent-file-override
          consult--source-project-file-hidden))
  ;; :general
  ;; ([remap switch-to-buffer]    'consult-buffer
  ;;  [remap goto-line]           'consult-goto-line
  ;;  [remap imenu]               'consult-imenu)
  ;; (tyrant-def
  ;;   "jI" '("imenu-multi" . consult-imenu-multi)
  ;;   "fl" '("locate-files" . consult-find)
  ;;   "jj" '("search lines" . consult-line)
  ;;   "jJ" '("search lines a/ buffers" . consult-line-multi)
  ;;   "Tt" 'consult-minor-mode-menu)
  ;; (org-mode-map
  ;;  [remap consult-imenu]       'consult-org-heading
  ;;  [remap consult-imenu-multi] 'consult-org-agenda)
  )

;;;; wgrep

(use-package wgrep  :defer t)

;;;; avy

(use-package avy
  :config
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        ;; the unpredictability of this (when enabled) makes it a poor default
        avy-single-candidate-jump nil)
  (add-to-list 'avy-ignored-modes 'magit-status-mode)
  )

;;;; embark

(use-package embark
  :after avy
  :init
  (with-eval-after-load 'avy
    (defun avy-action-embark (pt)
      (unwind-protect
          (save-excursion
            (goto-char pt)
            (embark-act))
        (select-window
         (cdr (ring-ref avy-ring 0))))
      t)
    (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))
  :config
  (with-eval-after-load 'which-key
    (defun embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
      The which-key help message will show the type and value of the
      current target followed by an ellipsis if there are further
      targets."
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (caar targets) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))

    (setq embark-indicators '(embark-which-key-indicator
                              embark-highlight-indicator
                              embark-isearch-highlight-indicator))

    (defun embark-hide-which-key-indicator (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      (when-let ((win (get-buffer-window which-key--buffer
                                         'visible)))
        (quit-window 'kill-buffer win)
        (let ((embark-indicators (delq #'embark-which-key-indicator embark-indicators)))
          (apply fn args))))

    (advice-add #'embark-completing-read-prompter
                :around #'embark-hide-which-key-indicator))

  (with-eval-after-load 'vertico
    (defun embark-vertico-indicator ()
      (let ((fr face-remapping-alist))
        (lambda (&optional keymap _targets prefix)
          (when (bound-and-true-p vertico--input)
            (setq-local face-remapping-alist
                        (if keymap
                            (cons '(vertico-current . embark-target) fr)
                          fr))))))

    (add-to-list 'embark-indicators #'embark-vertico-indicator))
  ;; :general
  ;; (:keymaps '(global normal)
  ;;           "C-." 'embark-act
  ;;           "M-." 'embark-dwim)
  )

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;; tempel

;; Template-based in-buffer completion (tempel.el)
;; NOTE 2023-01-19: Check the `templates'
(use-package tempel
  :ensure t
  :config
  ;; (setq tempel-trigger-prefix "<") ; conflits with evil-shift
  (setq tempel-path (expand-file-name "tempel-templates.eld" user-emacs-directory))
  ;; Use concrete keys because of org mode
  ;; "M-RET" #'tempel-done
  ;; "M-{" #'tempel-previous
  ;; "M-}" #'tempel-next
  ;; "M-<up>" #'tempel-previous
  ;; "M-<down>" #'tempel-next

  ;; 2023-10-19 disable my custom
  (define-key tempel-map (kbd "RET") #'tempel-done)
  (define-key tempel-map (kbd "M-n") #'tempel-next)
  (define-key tempel-map (kbd "M-p") #'tempel-previous)

  (global-set-key (kbd "M-+") 'tempel-complete)
  (global-set-key (kbd "M-*") 'tempel-insert)

  ;; 2023-10-19 disable my custom
  (define-key tempel-map (kbd "RET") #'tempel-done)
  (define-key tempel-map (kbd "M-n") #'tempel-next)
  (define-key tempel-map (kbd "M-p") #'tempel-previous)
  )

(use-package tempel-collection
  :defer t
  :after tempel
  )

;;;; corfu

;; TAB-and-Go customizations
;; https://github.com/minad/corfu?tab=readme-ov-file#tab-and-go-completion
(use-package corfu
  :demand t
  :ensure t
  :custom
  (corfu-cycle t)  ;; Enable cycling for `corfu-next/previous'
  ;; (corfu-auto-prefix 4) ; default 3
  (corfu-auto t) ;; default nil
  (corfu-on-exact-match nil)
  (corfu-min-width 35)
  (corfu-max-width 80)
  ;; (corfu-preselect 'prompt) ;; Always preselect the prompt
  :bind
  (:map corfu-map
        ("M-." . corfu-move-to-minibuffer))
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent t)

  (with-eval-after-load 'eldoc
    (eldoc-add-command #'corfu-insert))

  (require 'corfu-echo)
  (add-hook 'corfu-mode-hook 'corfu-echo-mode)
  (require 'corfu-history)
  (add-hook 'corfu-mode-hook 'corfu-history-mode)

  (global-corfu-mode)
  )

;;;; cape

(use-package cape
  :after corfu
  :demand t
  :init
  ;; /gopar-dotfiles-youtuber/README.org:1371
  (setq cape-dabbrev-min-length 4) ; default 4
  (setq cape-dabbrev-check-other-buffers 'some)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (setq-local corfu-auto nil) ;; Enable/disable auto completion
      ;; Disable automatic echo and popup
      (setq-local corfu-echo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-p" . completion-at-point) ;; capf
         ("M-P t" . complete-tag)        ;; etags
         ("M-P d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-P h" . cape-history)
         ("M-P f" . cape-file)
         ("M-P k" . cape-keyword)
         ("M-P s" . cape-elisp-symbol)
         ("M-P e" . cape-elisp-block)
         ("M-P a" . cape-abbrev)
         ("M-P l" . cape-line)
         ("M-P w" . cape-dict)
         ("M-P :" . cape-emoji)
         )
  )

;;;; magit

;; (unless (package-installed-p 'with-editor)
;;   (package-vc-install "https://github.com/magit/with-editor"))
(use-package with-editor)

(use-package magit
  :demand t
  :commands (magit-status
             magit-dispatch-popup
             magit-blame-addition
             magit-log-buffer-file)
  :bind (("C-x g" . magit-status)
         ("C-x M-g" . magit-dispatch-popup)))

;; (use-package forge
;;   :after magit)

;;;; outli

(unless (package-installed-p 'outli)
  (package-vc-install "https://github.com/jdtsmith/outli"))

(progn
  (require 'outli)

  ;; :vc (outli :url "https://github.com/jdtsmith/outli")
  (setq outli-speed-commands nil)
  ;; (add-to-list 'outli-heading-config '(tex-mode "%%" ?% t))
  (add-to-list 'outli-heading-config '(js2-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(js-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(typescript-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(typescript-ts-mode "//" ?\/ t))
  (add-to-list 'outli-heading-config '(python-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(python-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(awk-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(awk-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(elixir-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(elixir-ts-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(sh-mode "##" ?# t))
  (add-to-list 'outli-heading-config '(bash-ts-mode "##" ?# t))

  (add-to-list 'outli-heading-config '(clojure-mode ";;" ?\; t))
      (add-to-list 'outli-heading-config '(clojurescript-mode ";;" ?\; t))

      (add-hook 'prog-mode-hook 'outli-mode) ; not markdown-mode!
      ;; (add-hook 'org-mode-hook 'outli-mode)
      )

;;;; pcre2el

(use-package pcre2el)

;;;; which-key

(require 'which-key)
(setq which-key-popup-type 'minibuffer)
(setq which-key-idle-delay 0.4
      which-key-idle-secondary-delay 0.01
      which-key-allow-evil-operators t)
(setq which-key-sort-order #'which-key-key-order-alpha
      which-key-sort-uppercase-first nil
      which-key-add-column-padding 1
      which-key-max-display-columns nil
      which-key-min-display-lines 6
      which-key-side-window-slot -10)

;; (setq-default line-spacing 3) ; use fontaine

;;;; doom-modeline

;; (size-indication-mode t)
;; (column-number-mode t)

(use-package doom-modeline
  :hook (doom-modeline-mode . size-indication-mode) ; filesize in modeline
  :hook (doom-modeline-mode . column-number-mode)   ; cursor column in modeline
  :init
  (setq doom-modeline-time nil)
  (setq doom-modeline-time-icon nil)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-battery nil)
  (setq doom-modeline-support-imenu t)

  (setq doom-modeline-enable-word-count nil)
  ;; (setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mod)) ; org-mode

  (setq doom-modeline-icon nil) ; (display-graphic-p))

  (setq doom-modeline-modal-icon nil)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-buffer-modification-icon t)

  ;; (setq doom-modeline-height 35)
  (setq doom-modeline-bar-width 10)

  (setq doom-modeline-persp-name t) ; doom nil
  ;; (setq doom-modeline-window-width-limit (- fill-column 5))

  (setq doom-modeline-repl t)
  (setq doom-modeline-github t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-indent-info t)
  (setq doom-modeline-hud t)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

  :config
  ;; Fix an issue where these two variables aren't defined in TTY Emacs on MacOS
  (defvar mouse-wheel-down-event nil)
  (defvar mouse-wheel-up-event nil)

  (add-hook 'after-load-theme-hook #'doom-modeline-refresh-bars)
  (remove-hook 'display-time-mode-hook #'doom-modeline-override-time)
  (remove-hook 'doom-modeline-mode-hook #'doom-modeline-override-time)
  )

;;;; expand-region

(use-package expand-region
  :commands (er/contract-region er/mark-symbol er/mark-word)
  :config
  ;; Easily navigate sillycased words
  (global-subword-mode +1)
  (setq expand-region-contract-fast-key "V"
        expand-region-reset-fast-key "r"
        expand-region-subword-enabled t))

;;;; hydra

(use-package hydra :ensure t)
(use-package major-mode-hydra :ensure t)
(use-package pretty-hydra :ensure t)

;;;; aggressive-indent

(use-package aggressive-indent
  :defer 1
  :if window-system
  :config
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
  ;; (add-hook 'hy-mode-hook 'aggressive-indent-mode)
  )

;;;; puni

(use-package puni
  :diminish ""
  :hook ((puni-mode  . electric-pair-mode)
         (prog-mode  . puni-mode))
  :init
  ;; The default `puni-mode-map' respects "Emacs conventions".  We don't, so
  ;; it's better to simply clear and rewrite it.
  (setcdr puni-mode-map nil)
  (bind-keys
   :map puni-mode-map

   ;; ("M-<backspace>" . puni-splice)
   ("M-<delete>" . puni-splice) ; sp-unwrap-sexp

   ("C-<right>"  .  puni-slurp-forward)
   ("C-<left>" . puni-barf-forward)

   ("C-M-<left>" .  puni-slurp-backward)
   ("C-M-<right>" . puni-barf-backward)

   ("C-M-<delete>" . puni-splice-killing-forward)
   ("C-M-<backspace>" . puni-splice-killing-backward)

   ("C-M-a" . beginning-of-defun) ; default
   ("C-M-e" . end-of-defun)
   ("M-]" . forward-sexp) ; default
   ("M-[" . backward-sexp)

   ("C-M-f" . puni-forward-sexp)
   ("C-M-b" . puni-backward-sexp)

   ("C-M-p" . puni-beginning-of-sexp)
   ("C-M-n" . puni-end-of-sexp)

   ;; C-M-d down-sexp
   ("C-M-t" . transpose-sexp)
   ("C-M-?" . puni-convolute)

   ("C-M-k" . kill-sexp)
   ("C-M-K"   . backward-kill-sexp)
   ;; ("C-" . puni-backward-kill-word)

   ("M-)" . puni-syntactic-forward-punct)
   ("M-(" . puni-syntactic-backward-punct)

   ("C-c DEL" . puni-force-delete)
   ;; ("C-M-d" . puni-forward-delete-char)
   ;; ("C-M-k" . puni-kill-line)
   ;; ("C-M-K" . puni-backward-kill-line)
   ;;  ("C-M-w" . puni-kill-region)

   ;; ([remap puni-backward-kill-word] . backward-kill-word)
   ("C-M-z" . puni-squeeze) ; unwrap

   ("C-c {" . puni-wrap-curly)
   ("C-c (" . puni-wrap-round)
   ("C-c [" . puni-wrap-square)
   )
  )

;;;; winum

(use-package winum
  :init
  (setq winum-scope                      'frame-local
	winum-auto-assign-0-to-minibuffer t
	winum-reverse-frame-list          nil
	winum-auto-setup-mode-line nil
	winum-ignored-buffers '(" *LV*" " *which-key*"))
  :config
  (define-key winum-keymap (kbd "M-0") 'winum-select-window-0-or-10)
  (define-key winum-keymap (kbd "M-1") 'winum-select-window-1)
  (define-key winum-keymap (kbd "M-2") 'winum-select-window-2)
  (define-key winum-keymap (kbd "M-3") 'winum-select-window-3)
  (define-key winum-keymap (kbd "M-4") 'winum-select-window-4)
  (define-key winum-keymap (kbd "M-5") 'winum-select-window-5)
  (define-key winum-keymap (kbd "M-6") 'winum-select-window-6)
  (define-key winum-keymap (kbd "M-7") 'winum-select-window-7)
  (define-key winum-keymap (kbd "M-8") 'winum-select-window-8)
  (define-key winum-keymap (kbd "M-9") 'winum-select-window-9)

  (define-key winum-keymap
	      [remap winum-select-window-9] #'switch-to-minibuffer-window)

  (winum-mode 1)
  )

;;;; celestial-mode-line

(use-package celestial-mode-line
  :after time
  :init
  (setq celestial-mode-line-update-interval 3600) ; default 60
  (setq celestial-mode-line-sunrise-sunset-alist '((sunrise . "🌅") (sunset . "🌄")))
  (setq celestial-mode-line-phase-representation-alist
        '((0 . "🌚")(1 . "🌛")(2 . "🌝")(3 . "🌜")))
  :config
  (celestial-mode-line-start-timer)
  )

;;;; keycast

(use-package keycast
  :config
  ;; (setq keycast-tab-bar-minimal-width 50) ; 40
  ;; (setq keycast-tab-bar-format "%10s%k%c%r")

  (dolist (input '(self-insert-command
                   org-self-insert-command))
    (add-to-list 'keycast-substitute-alist `(,input "." "Typing…")))
  (dolist (event '(mouse-event-p
                   mouse-movement-p
                   mwheel-scroll
                   handle-select-window
                   mouse-set-point mouse-drag-region
                   dired-next-line ; j
                   dired-previous-line ; k
                   next-line
                   previous-line
                   evil-next-line ; j
                   evil-previous-line ; k
                   evil-forward-char ; l
                   evil-backward-char ; h
                   pixel-scroll-interpolate-up ; <prior> page-up
                   pixel-scroll-interpolate-down ; <next> page-down

                   toggle-input-method
                   block-toggle-input-method
                   evil-formal-state
                   evil-force-normal-state

                   ;; 2023-10-02 Added for clojure-dev
                   lsp-ui-doc--handle-mouse-movement
                   ignore-preserving-kill-region
                   ;; pdf-view-text-region
                   ;; pdf-view-mouse-set-region
                   ;; mouse-set-region
                   ))
    (add-to-list 'keycast-substitute-alist `(,event nil)))
  )

;;;###autoload
(defun my/load-global-mode-string ()
  (interactive)

  (when (not (bound-and-true-p display-time-mode))
    (display-time-mode t))

  (setq global-mode-string (remove 'display-time-string global-mode-string))
  (setq global-mode-string '("" celestial-mode-line-string display-time-string))

  (doom-modeline-mode +1)
  (keycast-tab-bar-mode +1)
  )

(add-hook 'after-init-hook #'my/load-global-mode-string)

;;;; remember (built-in)

(use-package remember
  :ensure nil
  :defer 2
  :commands remember)

;; :config
;; (setq remember-data-file (my/org-remember-file))
;; (setq remember-notes-initial-major-mode 'org-mode
;;       remember-notes-auto-save-visited-file-name t)

;;;; Load Extra files

;; (load-file (concat (file-name-as-directory user-emacs-directory) "meow.el"))
(load-file (concat (file-name-as-directory user-emacs-directory) "extra.el"))
(load-file (concat (file-name-as-directory user-emacs-directory) "core-funcs.el"))
;; (load-file (concat (file-name-as-directory user-emacs-directory) "org-config.el"))

;;; Note-Tacking

(use-package denote-sections)

;;;; DONT org-node

;; (unless (package-installed-p 'org-node)
;;   (package-vc-install "https://github.com/meedstrom/org-node"))

;; (use-package org-node
;;   :hook (org-mode . org-node-cache-mode))

;; (global-set-key (kbd "M-s f") #'org-node-find)
;; (global-set-key (kbd "M-s i") #'org-node-insert-link)


;;; IDE

;;;; treesit-auto

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;; combobulate

;; `M-x combobulate' (default: `C-c o o') to start using Combobulate
;; (setq c-ts-mode-indent-offset 4)
(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode))
  :config
  ;; Do not forget to customize Combobulate to your liking:
  ;;  M-x customize-group RET combobulate RET
  )

(unless (package-installed-p 'combobulate)
  (package-vc-install "https://github.com/mickeynp/combobulate"))

(use-package combobulate
  :ensure nil
  :preface
  ;; You can customize Combobulate's key prefix here.
  ;; Note that you may have to restart Emacs for this to take effect!
  (setq combobulate-key-prefix "C-c o")
  :hook
  ((python-ts-mode . combobulate-mode)
   (js-ts-mode . combobulate-mode)
   (html-ts-mode . combobulate-mode)
   (css-ts-mode . combobulate-mode)
   (yaml-ts-mode . combobulate-mode)
   (typescript-ts-mode . combobulate-mode)
   (json-ts-mode . combobulate-mode)
   (tsx-ts-mode . combobulate-mode))
  ;; Amend this to the directory where you keep Combobulate's source
  ;; code.
  )

;;;; evil-textobj-tree-sitter

(use-package evil-textobj-tree-sitter
  :after treesit
  :config
  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  ;; You can also bind multiple items and we will match the first one we can find
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
  )

;;;; eglot

(use-package eglot
  :ensure nil
  :demand t
  :commands eglot
  :bind (:map eglot-mode-map
              ("C-c d" . eldoc)
              ("C-c a" . eglot-code-actions)
              ("C-c r" . eglot-rename))
  :init
  (progn
    (setq eglot-autoshutdown t) ;; shutdown after closing the last managed buffer
    ;; (setq eglot-sync-connect 0) ;; async, do not block
    ;; (setq eglot-extend-to-xref t) ;; can be interesting!
    ;; (setq eglot-send-changes-idle-time 0.7)
    )
  :config
  ;; (add-to-list 'eglot-server-programs '(elixir-ts-mode "language_server.sh"))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  ;; whhone
  (define-key eglot-mode-map
              [remap xref-find-definitions] #'eglot-find-typeDefinition)
  (define-key eglot-mode-map
              [remap xref-find-references] #'eglot-find-declaration)
  (define-key eglot-mode-map (kbd "M-l r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "M-l f") 'eglot-format)

  ;; :config
  ;; Provide `consult-lsp' functionality from `consult-eglot', useful
  ;; for packages which relay on `consult-lsp' (like `dirvish-subtree').
  ;; (defalias 'consult-lsp-file-symbols #'consult-eglot-symbols)
  ;; (define-key eglot-mode-map (kbd "C-c e c") #'consult-eglot-symbols)
  )

(use-package consult-eglot
  :after eglot
  :bind (:map eglot-mode-map
              ("C-c s" . consult-eglot-symbols)))

;;;; conda

;; (unless (package-installed-p 'conda)
;;   (package-install 'conda))

(use-package conda)

(require 'conda)
;; if you want interactive shell support, include:
(conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
;; (conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
(conda-env-autoactivate-mode t)
;; if you want to automatically activate a conda environment on the opening of a file:
(add-to-list 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
                                          (conda-env-activate-for-buffer))))

;;;; hylang

(unless (package-installed-p 'hy-mode)
  (package-vc-install "https://github.com/jethack23/hy-mode"))

(use-package hy-mode
  :ensure nil
  :mode "\\.hy\\'"
  :interpreter "hy"
  ;; :hook ((hy-mode . eglot-ensure))
  :config
  ;; (set-repl-handler! 'hy-mode #'hy-shell-start-or-switch-to-shell)
  ;; (set-formatter! 'lisp-indent #'apheleia-indent-lisp-buffer :modes '(hy-mode))
  (when (executable-find "hyuga") ; it's works!
    (with-eval-after-load 'eglot
      (add-to-list 'eglot-server-programs '(hy-mode . ("hyuga"))))
    )
  )

;;;; haskell

(use-package haskell-mode)

;;;; formatter

;;;###autoload
(defun my/format-buffer ()
  "Format a buffer."
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (indent-region (point-min) (point-max)))
   ((eq major-mode 'ledger-mode)
    (ledger-mode-clean-buffer))
   (t (call-interactively 'apheleia-format-buffer))))

(use-package apheleia
  :after evil
  :commands (apheleia-format-buffer my/format-buffer)
  :config
  ;; (add-hook 'markdown-mode-hook 'apheleia-mode)
  (add-hook 'yaml-mode-hook 'apheleia-mode)
  )

;;;; TODO sly for common-lisp

;;;; emacs-lisp-mode-hook

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  ;; Emacs' built-in elisp files use a hybrid tab->space indentation scheme
                                  ;; with a tab width of 8. Any smaller and the indentation will be
                                  ;; unreadable. Since Emacs' lisp indenter doesn't respect this variable it's
                                  ;; safe to ignore this setting otherwise.
                                  ;; (setq-local tab-width 8)
                                  (setq-local comment-column 0)
                                  (define-key emacs-lisp-mode-map (kbd "M-[") 'backward-sexp)
                                  (define-key emacs-lisp-mode-map (kbd "M-]") 'forward-sexp)))

;;; Journal 

;;;; org-journal

(use-package org-journal
  :commands (org-journal-new-entry org-journal-search-forever)
  :config
  (setq org-journal-dir (concat user-org-directory "journal"))
  (setq org-journal-date-format "%Y-%m-%d(%a)")
  (setq org-journal-file-format "%Y%m%dT000000--%Y-%m-%d__journal.org")
  (setq org-journal-time-prefix "* ")
  (setq org-journal-date-prefix "#+title: ")
  ;; org-journal-skip-carryover-drawers (list "LOGBOOK")
  ;; (setq org-journal-enable-agenda-integration t)
  )

;; #+date: [%<%Y-%m-%d %a %H:%M>]\n#+filetags: :journal:\n#+identifier: %<%Y%m%dT000000>\n#+description:\n#+category: Journal\n#+startup: fold\n# #+glossary_sources: global
;; #+BEGIN: clocktable :scope agenda :maxlevel 2 :step day :fileskip0 true :tstart \"%<%Y-%m-%d>\" :tend \"%(my/tomorrow)\"
;; #+END:

;; (defun org-journal-file-header-func (time)
;;   "Custom function to create journal header."
;;   (concat
;;    (pcase org-journal-file-type
;;      (`daily "#+TITLE: Daily Journal\n#+STARTUP: showeverything"
;;              (`weekly "#+TITLE: Weekly Journal\n#+STARTUP: folded")
;;              (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: folded")
;;              (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: folded")))))
;; (setq org-journal-file-header 'org-journal-file-header-func)

;;;; org-journal-tags

;; (use-package org-journal-tags
;;   :after (org-journal)
;;   :config
;;   (org-journal-tags-autosync-mode))

;;; Keybindings

;;;; For Mode

(load-file (concat (file-name-as-directory user-emacs-directory) "keys.el"))

;;;; Hydra and Transient

(load-file (concat (file-name-as-directory user-emacs-directory) "hydrakeys.el"))

;;;; corkey bindings

(dolist (dir '("corkey" "corgi-bindings"))
  (push (expand-file-name dir user-emacs-directory) load-path))

(message "Loading corgi-bindings...")
(require 'corgi-bindings)

;; Corgi's keybinding system, which builds on top of Evil. See the manual, or
;; visit the key binding and signal files (with `SPC f e k', `SPC f e K', `SPC
;; f e s' `SPC f e S')
;; Put this last here, otherwise keybindings for commands that aren't loaded
;; yet won't be active.

(message "Loading corkey...")
(require 'corkey)
(corkey-mode 1)
;; Automatically pick up keybinding changes
(corkey/load-and-watch)

;;; Easy mode

;;;; context-mode 

;;;; menu-bar

(menu-bar-mode 1)

;;;; casual-suite
(use-package casual-suite
  :defer 1
  :config
  (define-key Info-mode-map (kbd "<f2>") #'casual-info-tmenu)
  (global-set-key (kbd "M-g v") 'casual-avy-tmenu)
  (define-key dired-mode-map (kbd "<f2>") #'casual-dired-tmenu)
  (define-key calc-mode-map (kbd "<f2>") #'casual-calc-tmenu)
  (define-key isearch-mode-map (kbd "<f2>") #'casual-isearch-tmenu)
  ;; ibuffer-mode-map
  )

;;; Workspaces

(defun +my/open-workspaces ()
  (interactive)
  (when (= 1 (length (tab-bar-tabs)))
    (tab-bar-new-tab)
    (tab-bar-rename-tab "home" 1)
    (tab-bar-rename-tab "time" 2)
    ;; (tab-bar-select-tab 2)
    ;; (org-agenda nil "n")
    ;; (org-agenda-goto-today)
    ;; (delete-other-windows)
    (tab-bar-select-tab 1)
    )
  )

(+my/open-workspaces)

;;; UI

;;;; modus-themes

(use-package modus-themes
  :config
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-custom-auto-reload t

        ;; Options for `modus-themes-prompts' are either nil (the
        ;; default), or a list of properties that may include any of those
        ;; symbols: `italic', `WEIGHT'
        ;; modus-themes-prompts '(bold)

        ;; The `modus-themes-completions' is an alist that reads two
        ;; keys: `matches', `selection'.  Each accepts a nil value (or
        ;; empty list) or a list of properties that can include any of
        ;; the following (for WEIGHT read further below):
        ;; `matches'   :: `underline', `italic', `WEIGHT'
        ;; `selection' :: `underline', `italic', `WEIGHT'
        ;; modus-themes-completions
        ;; '((matches   . (semibold))
        ;;   (selection . (semibold text-also)))

        modus-themes-common-palette-overrides
        `((fg-mode-line-active fg-main) ; Black

          ;; Comments are yellow, strings are green
          (comment yellow-cooler)
          (string green-warmer)

          ;; "Make the mode line borderless"
          (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)

          ;; "Make matching parenthesis more or less intense"
          (bg-paren-match bg-magenta-intense)
          (underline-paren-match unspecified)

          ;; Intense magenta background combined with the main foreground
          ;; (bg-region bg-magenta-subtle)
          ;; (fg-region fg-main)

          ;; Links
          ;; (underline-link border)
          ;; (underline-link-visited border)
          ;; (underline-link-symbolic border)

          (bg-heading-0 bg-green-subtle) ; green
          (bg-heading-1 bg-dim)
          (bg-heading-2 bg-yellow-subtle)
          (bg-heading-3 bg-blue-nuanced) ; blue

          ;; copy from intense
          (overline-heading-0 unspecified)
          (overline-heading-1 magenta-cooler)
          (overline-heading-2 magenta-warmer)

          ;; And expand the preset here. Note that the ,@ works because we use
          ;; the backtick for this list, instead of a straight quote.
          ;; ,@modus-themes-preset-overrides-faint
          ;; ,@modus-themes-preset-overrides-intense
          ))

  (defun my/modus-themes-custom-faces ()
    (interactive)
    ;; (message "modus-themes-after-hook : my-modus-themes-custom-faces")
    (modus-themes-with-colors
      (custom-set-faces
       ;; `(tab-bar ((,c :background ,bg-tab-bar)))
       ;; `(tab-bar-tab-group-current ((,c :inherit bold :background ,bg-tab-current :box (:line-width -2 :color ,bg-tab-current) :foreground ,fg-alt)))
       ;; `(tab-bar-tab-group-inactive ((,c :background ,bg-tab-bar :box (:line-width -2 :color ,bg-tab-bar) :foreground ,fg-alt)))
       ;; `(tab-bar-tab ((,c :inherit bold :box (:line-width -2 :color ,bg-tab-current) :background ,bg-tab-current)))
       ;; `(tab-bar-tab-inactive ((,c :box (:line-width -2 :color ,bg-tab-other) :background ,bg-tab-other)))
       ;; `(tab-bar-tab-ungrouped ((,c :inherit tab-bar-tab-inactive)))
       ;; `(fringe ((,c :background ,bg-dim)))

       `(vterm-color-black ((,c :background "gray25" :foreground "gray25")))
       `(vterm-color-yellow ((,c :background ,yellow-intense :foreground ,yellow-intense)))
       `(org-mode-line-clock ((,c :inherit bold :foreground ,modeline-info)))
       `(org-mode-line-clock-overrun ((,c :inherit bold :foreground ,modeline-err)))
       `(jinx-misspelled ((,c :underline (:style wave :color ,magenta-cooler))))
       ;; `(keycast-command ((,c :inherit default :height 0.9)))
       ))
    (when (display-graphic-p) ; gui
      (when (locate-library "spacious-padding")
        (spacious-padding-mode +1)))
    )
  (add-hook 'modus-themes-post-load-hook #'my/modus-themes-custom-faces)

  (load-theme 'modus-operandi :no-confirm))

;;;; disable scroll-bar-mode

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;;; paren and pair
;;;; show-paren-mode/electric-pair-mode and customize for org-mode

;; 2023-11-10 puni + electric-pair 사용 중. 이걸 꺼야 org-block 에서 문제가 없다.
;; 2023-09-28 아니다. 켜 놓은 이유가 있을 것. elctric-pair 가 아니지 않는가?
;; 스페이스맥스에서 왜 이걸 켜 놓는 것인가?! 일단 끈다.
;; C-j 누르면 electric-newline-and-maybe-indent 수행. indent 가 안맞는다. 필요 없다.
;; (electric-indent-mode -1) ; important!! 이렇게 따로 꺼야 한다.

;; https://github.com/alphapapa/smart-tab-over
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html
;; 괄호만 강조
(setq show-paren-style 'parenthesis) ; default 'parenthesis
;; 괄호 입력 후 내용 입력시 괄호를 강조
(setq show-paren-when-point-inside-paren t)
;; (setq show-paren-when-point-in-periphery t)

;; 괄호 강조를 즉시 보여준다
(use-package paren
  :ensure nil
  :hook (prog-mode . +show-paren-mode)
  :config
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t
        show-paren-when-point-in-periphery nil)
  (defun +show-paren-mode()
    (unless show-paren-mode (show-paren-mode))))

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Matching.html
;; 괄호, 구분자(delimiter) 자동 쌍 맞추기
(setq electric-pair-pairs '((?\{ . ?\})
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\" . ?\")))

;; from Crafted-Emacs - crafted-org-config.el
;; Disable auto-pairing of "<" or "[" in org-mode with electric-pair-mode
(defun my/org-enhance-electric-pair-inhibit-predicate ()
  "Disable auto-pairing of \"<\" or \"[\" in `org-mode' when using `electric-pair-mode'."
  (when (and electric-pair-mode (eql major-mode #'org-mode))
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c)
                   (if (or (char-equal c ?<)
                           (char-equal c ?\[ ))
                       t (,electric-pair-inhibit-predicate c))))))

;; Add hook to both electric-pair-mode-hook and org-mode-hook
;; This ensures org-mode buffers don't behave weirdly,
;; no matter when electric-pair-mode is activated.
(add-hook 'electric-pair-mode-hook #'my/org-enhance-electric-pair-inhibit-predicate)
(add-hook 'org-mode-hook #'my/org-enhance-electric-pair-inhibit-predicate)

;;;; Corfu and electric-Pair and Jump In/Out Parens

;; Linux GUI : <tab> TAB
;; Linux Terminal : TAB
;; Linux GUI : S-<iso-lefttab>
;; Linux Terminal : <backtab>

;;;###autoload
(defun jump-out-of-pair ()
  (interactive)
  (let ((found (search-forward-regexp "[])}\"'`*=]" nil t)))
    (when found
      (cond ((or (looking-back "\\*\\*" 2)
  		 (looking-back "``" 2)
  		 (looking-back "\"\"" 2) ; 2023-10-02 added
  		 (looking-back "''" 2)
  		 (looking-back "==" 2))
  	     (forward-char))
  	    (t (forward-char 0))))))
;; 절대 하지 말것! (global-set-key [remap indent-for-tab-command] #'jump-out-of-pair)

;;;###autoload
(defun jump-backward-pair ()
  (interactive)
  (let ((found (search-backward-regexp "[])}\"'`*=]" nil t)))
    (when found
      (cond ((or (looking-back "\\*\\*" 2)
                 (looking-back "``" 2)
                 (looking-back "\"\"" 2) ; 2023-10-02 added
                 (looking-back "''" 2)
                 (looking-back "==" 2))
             (backward-char))
            (t (backward-char 0))))))


;;; init.el ends here
