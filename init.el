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

;; (setq debug-on-error t)

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
  (warning-minimum-level :emergency)
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
  ;; (modus-themes-mixed-fonts t)
  (modus-themes-to-toggle
   '(modus-operandi modus-vivendi-tinted))
  ;; :init
  ;; (load-theme 'modus-operandi :no-confirm)
  :bind
  (("C-c w t t" . modus-themes-toggle)
   ("C-c w t m" . modus-themes-select)
   ("C-c w t s" . consult-theme))
  :init
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

  :config
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
       ;; `(jinx-misspelled ((,c :underline (:style wave :color ,magenta-cooler))))
       ;; `(keycast-command ((,c :inherit default :height 0.9)))
       ))
    (when (display-graphic-p) ; gui
      (when (locate-library "spacious-padding")
        (spacious-padding-mode +1)))
    )
  (add-hook 'modus-themes-post-load-hook #'my/modus-themes-custom-faces)
  )

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
  (vertico-sort-function 'vertico-sort-history-alpha)
  )

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
  ;; :custom
  ;; (ispell-program-name "hunspell")
  ;; (ispell-dictionary ews-hunspell-dictionaries)
  ;; (flyspell-mark-duplications-flag nil) ;; Writegood mode does this
  ;; (org-fold-core-style 'overlays) ;; Fix Org mode bug
  ;; :config
  ;; (ispell-set-spellchecker-params)
  ;; (ispell-hunspell-add-multi-dic ews-hunspell-dictionaries)
  ;; :hook
  ;; (text-mode . flyspell-mode)
  :bind
  (("C-c w s s" . ispell)
   ;; ("C-;"       . flyspell-auto-correct-previous-word)
   ))

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

;; (use-package emms
;;   :if window-system
;;   ;; :when (eq system-type 'gnu/linux)
;;   :init
;;   (require 'emms-setup)
;;   (require 'emms-mpris)
;;   (emms-all)
;;   (emms-default-players)
;;   (emms-mpris-enable)
;;   :custom
;;   (emms-browser-covers #'emms-browser-cache-thumbnail-async)
;;   :bind
;;   (("C-c w m b" . emms-browser)
;;    ("C-c w m e" . emms)
;;    ("C-c w m p" . emms-play-playlist )
;;    ("<XF86AudioPrev>" . emms-previous)
;;    ("<XF86AudioNext>" . emms-next)
;;    ("<XF86AudioPlay>" . emms-pause)))

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
  ;; :custom
  ;; (consult-denote-find-command
  ;;  #'(lambda() (find-file (consult-denote-file-prompt))))
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

;;;; Path

(setq user-dotemacs-dir user-emacs-directory)

(setq emacs-type 'vanillaemacs)

(defun my/org-emacs-config-file () (expand-file-name "README.org" user-dotemacs-dir))

;; optimize: force "lisp"" and "site-lisp" at the head to reduce the startup time.
;; (dolist (dir '("menu" "site-lisp"))
;;   (push (expand-file-name dir user-dotemacs-dir) load-path))

;;;; Termux

(setq-default root-path "/")

(defvar IS-TERMUX
  (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

(when IS-TERMUX
  (setq root-path "/data/data/com.termux/files/"))

(setq my/slow-ssh
      (or
       (string= (getenv "IS_TRAMP") "true")))

(setq my/remote-server
      (or (string= (getenv "IS_REMOTE") "true")
          ;; (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a")))
          (string= (system-name) "server1")
          (string= (system-name) "server2")
          (string= (system-name) "server3"))) ; for test

(setenv "IS_EMACS" "true")

;;;; PGTK

;; You should be able to use input methods since GtkIMContext is enabled by
;; default. If you don't like GtkIMContext, you can disable it by writing as
;; follows in ~/.emacs: pgtk-use-im-context disable gtk im modules for
;; emacs-pgtk, add "Emacs*UseXIM: false" to ~/.Xresources to disable xim
(if (eq window-system 'pgtk)
    (pgtk-use-im-context nil))

(when (boundp 'pgtk-use-im-context-on-new-connection)
  (setq pgtk-use-im-context-on-new-connection nil))


;;;; Load 'Per-Machine'

;; Most of my per-environment config done via =customize= and is in .custom.el.
;; However, some config is more involved, such as packages I just want in one
;; environment and not the others.  To that end, let's load a file that can contain
;; those customizations.
(let ((per-machine-filename (concat user-emacs-directory "per-machine.el")))
  (when (file-exists-p per-machine-filename)
    (load-file per-machine-filename)))

;;;; Overide path and configs

(setq ews-hunspell-dictionaries "en_US") ; ko_KR
(setq denote-directory user-org-directory)

(setq ews-bibtex-directory (concat org-directory "library"))
(setq citar-bibliography (list (concat org-directory "library/emacs-writing-studio.bib")))

;; use #+cite_export: csl apa.csl
;; (setq org-cite-csl-styles-dir (concat user-org-directory ".csl"))
;; (setq citar-citeproc-csl-styles-dir (concat user-org-directory ".csl"))
;; (setq citar-citeproc-csl-locales-dir "~/.csl/locales")
(setq citar-citeproc-csl-style "apa.csl") ; ieee.csl
;; (setq citar-notes-paths (list (concat org-directory "bib/")))
(setq org-cite-global-bibliography citar-bibliography)

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

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; (setq ring-bell-function 'ignore)

;;;; minibuffer

(setq enable-recursive-minibuffers t)

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
(setq display-time-format "| %a %e %b, %H:%M |")
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
  :config
  (setq consult-narrow-key "<")
  (setq consult-preview-key '("M-." "C-SPC")) ; default any

  ;; default's C-s
  (define-key minibuffer-local-map (kbd "M-r") 'consult-history)
  (define-key read-expression-map (kbd "C-r") #'consult-history)
  (define-key read-expression-map (kbd "M-r") #'consult-history)

  (defun my/consult-find () (interactive) (consult-find "."))
  (defun my/consult-fd () (interactive) (consult-fd "."))

  ;; spacemacs/layers/+completion/compleseus/funcs.el
  (defun my/compleseus-search (use-initial-input initial-directory)
    (let* ((initial-input (if use-initial-input
                              (rxt-quote-pcre
                               (if (region-active-p)
                                   (buffer-substring-no-properties
                                    (region-beginning) (region-end))
                                 (or (thing-at-point 'symbol t) "")))
                            ""))
           (default-directory
            (or initial-directory (read-directory-name "Start from directory: "))))
      (consult-ripgrep default-directory initial-input)))

  (defun +default/search-cwd-symbol-at-point ()
    "Search current folder."
    (interactive)
    (my/compleseus-search t default-directory))

  (consult-customize
   +default/search-cwd-symbol-at-point
   :preview-key '("M-." "C-SPC"))

  (consult-customize
   consult-theme
   :preview-key '("M-." "C-SPC"
                  :debounce 3.0 any))

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
;; (use-package with-editor)

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

;;;; paren and pair
;;;;; show-paren-mode/electric-pair-mode and customize for org-mode

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

;;;;; corfu and electric-Pair and Jump In/Out Parens

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

  (when (string= (system-name)"jhnuc")
    (keycast-tab-bar-mode +1))
  )

(unless IS-TERMUX
  (add-hook 'after-init-hook #'my/load-global-mode-string))


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
(load-file (concat (file-name-as-directory user-emacs-directory) "org-funcs.el"))


;;;; formatter - apheleia

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


;;;; hl-todo

(use-package hl-todo 
  :hook (after-init . global-hl-todo-mode))

;;;; breadcrumb

(use-package breadcrumb
  :init
  ;; (add-hook 'prog-mode-hook 'breadcrumb-local-mode) ; conflict with lsp-mode
  (add-hook 'emacs-lisp-mode-hook 'breadcrumb-local-mode)
  ;; (add-hook 'markdown-mode-hook 'breadcrumb-local-mode)
  :custom
  (breadcrumb-project-max-length 0.1)
  (breadcrumb-imenu-max-length 0.2)
  )

;; (with-eval-after-load 'popup
;;   :config
;;   (define-key popup-menu-keymap (kbd "C-j") 'popup-next)
;;   (define-key popup-menu-keymap (kbd "C-k") 'popup-previous)
;;   (define-key popup-menu-keymap (kbd "C-n") 'popup-next)
;;   (define-key popup-menu-keymap (kbd "C-p") 'popup-previous))

;;; IDE - unless is-termux


;;;; START 

(when (eq system-type 'gnu/linux)
  (unless IS-TERMUX
    
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

    (use-package conda)
    (require 'conda)
    ;; if you want interactive shell support, include:
    (conda-env-initialize-interactive-shells)
    ;; if you want eshell support, include:
    ;; (conda-env-initialize-eshell)
    ;; if you want auto-activation (see below for details), include:
    (conda-env-autoactivate-mode t)
    ;; if you want to automatically activate a conda environment on the opening of a file:
    (add-to-list 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path))
                                   (conda-env-activate-for-buffer)))
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

;;;; TODO sly for common-lisp

;;;; END
    )
  )

;;; Note-Tacking


;;;; use-package

(use-package denote-sections)
(use-package org-pomodoro)
(use-package org-cliplink)
;; (use-package org-download)
;; (use-package async)

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
;; (use-package org-journal-tags
;;   :after (org-journal)
;;   :config
;;   (org-journal-tags-autosync-mode))

;;;; Load Org-mode

(with-eval-after-load 'org

  (global-unset-key (kbd "<f10>"))

  (setq org-crypt-key (car epa-file-encrypt-to))
  (message "`org-directory' has been set to: %s" org-directory)

  ;; Adding a "/" so that =find-file= finds the files under =~/org/=.
  ;;
  ;; The org-directory is computed based on user-emacs-directory.
  ;; - ".emacs.d" -> "~/org/"
  ;; - ".emacs.d-personal" -> "~/org-personal/"
  ;;(concat "~/org" (nth 1 (split-string user-emacs-directory "emacs.d"))))
  ;; (setq org-directory
  ;;       (concat "~/org" (nth 1 (split-string dotspacemacs-directory
  ;;                                            "spacemacs.d"))))

;;;; My Style

  (setq org-enforce-todo-dependencies t)
  (setq org-cycle-separator-lines 0)

  (setq org-insert-heading-respect-content nil)

  ;; 리버스 순서가 익숙하다.
  (setq org-reverse-note-order t) ; default nil

  (setq org-show-following-heading t)
  (setq org-show-hierarchy-above t)

  (setq org-special-ctrl-a/e t) ; doom t
  (setq org-special-ctrl-k nil) ; doom nil
  (setq org-yank-adjusted-subtrees t)

  ;; 22/10/11--22:18 :: headline 설정 좋다.
  ;; (setq org-fontify-todo-headline nil) ; default nil
  ;; (setq org-fontify-done-headline nil) ; doom t
  (setq org-fontify-whole-heading-line nil) ; doom t

  (defvar bh/insert-inactive-timestamp t)

  (defun bh/toggle-insert-inactive-timestamp ()
    (interactive)
    (setq bh/insert-inactive-timestamp (not bh/insert-inactive-timestamp))
    (message "Heading timestamps are %s" (if bh/insert-inactive-timestamp "ON" "OFF")))

  (defun bh/insert-inactive-timestamp ()
    (interactive)
    (org-insert-time-stamp nil t t nil nil nil))

  (defun bh/insert-heading-inactive-timestamp ()
    (save-excursion
      (when bh/insert-inactive-timestamp
        (org-return)
        (org-cycle)
        (bh/insert-inactive-timestamp))))

  ;; 2024-02-24 turn-off
  ;; (add-hook 'org-insert-heading-hook 'bh/insert-heading-inactive-timestamp 'append)

  (setq org-return-follows-link t)

  (setq org-tags-match-list-sublevels t)
  (setq org-agenda-persistent-filter t)

  (setq org-export-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-charset-priority 'unicode)
  (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
  (setq org-time-clocksum-format
        '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)

  ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
  (setq org-clock-history-length 23) ; doom default 20
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t) ; doom default t

  ;; Separate drawers for clocking and logs
  (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
  ;; Save clock data and state changes and notes in the LOGBOOK drawer
  (setq org-clock-into-drawer t)
  ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t) ; doom default t
  ;; Clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (setq org-clock-persist t) ; doom 'history
  ;; Enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
  ;; Include current clocking task in clock reports
  (setq org-clock-report-include-clocking-task t)

  ;; ex) 2022-09-19 (월)
  (setq org-agenda-format-date "%Y-%m-%d (%a)")

  (defun bh/make-org-scratch ()
    (interactive)
    (find-file (concat org-directory "/scratch.org"))
    ;; (gnus-make-directory "/tmp/publish")
    )

  (defun bh/make-markdown-scratch ()
    (interactive)
    (find-file (concat org-directory "/md/scratch.md"))
    ;; (gnus-make-directory "/tmp/publish")
    )

  (defun bh/switch-to-scratch ()
    (interactive)
    (switch-to-buffer "*scratch*"))

  (defun ash-goto-org-agenda (&optional _)
    (interactive)
    (let ((buf (get-buffer "*Org Agenda(n)*")))
      (tab-bar-switch-to-tab "time")
      (if buf
          (progn (switch-to-buffer buf)
                 (delete-other-windows))
        (progn
          ;; (when (locate-library "org-roam")
          ;;   (require 'org-roam)
          ;;   (org-roam-db-sync))

          (org-agenda nil "n")
          (org-agenda-goto-today)
          (spacemacs/toggle-current-window-dedication) ; spacemacs Compatibility
          )
        )))

;;;; global map

  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c i") 'org-insert-link)
  ;; (global-set-key (kbd "C-c a") 'ash-goto-org-agenda)

  ;; Sets default-directory to org-directory so that =M-x magit= from the agenda view does not ask me for a dir.
  (global-set-key (kbd "C-c a") 'org-agenda)
  ;; (global-set-key (kbd "C-c A")
  ;;                 (lambda () (interactive) (let ((default-directory org-directory)) (org-agenda))))

  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c \\") 'org-tags-sparse-tree)

  ;; (global-unset-key (kbd "<f6>"))
  ;; (global-set-key (kbd "<C-f6>") #'(lambda () (interactive) (bookmark-set "SAVED")))
  ;; (global-set-key (kbd "<f6>") #'(lambda () (interactive) (bookmark-jump "SAVED")))

  (global-set-key (kbd "<f10> c") 'calendar)

  (global-set-key (kbd "<f10> o") 'bh/make-org-scratch)
  (global-set-key (kbd "<f10> O") 'bh/make-markdown-scratch)

  (global-set-key (kbd "<f10> s") 'bh/switch-to-scratch)

  (global-set-key (kbd "<f10> t") 'bh/insert-inactive-timestamp)
  (global-set-key (kbd "<f10> T") 'bh/toggle-insert-inactive-timestamp)

  (global-set-key (kbd "<f10> v") 'visible-mode)
  (global-set-key (kbd "<f10> l") 'org-toggle-link-display)
  (global-set-key (kbd "C-<f10>") 'previous-buffer)
  (global-set-key (kbd "M-<f10>") 'org-toggle-inline-images)
  (global-set-key (kbd "C-x n r") 'narrow-to-region)
  ;; (global-set-key (kbd "C-<f10>") 'next-buffer)

  (global-set-key (kbd "<f10> 9") 'org-clock-goto)
  (global-set-key (kbd "<f10> <f10>") 'org-clock-goto)

  (global-set-key (kbd "<f10> h") 'outline-hide-other)
  (global-set-key (kbd "<f10> k") 'my/get-id-to-clipboard)
  (global-set-key (kbd "<f10> r") 'remember)

  (define-key org-mode-map (kbd "C-M-i") 'completion-at-point)

  ;; with markdown-mode-map
  (define-key org-mode-map (kbd "<f10> g") 'org-toggle-inline-images)
  (define-key org-mode-map (kbd "<f10> m") 'my/org-toggle-emphasis-markers)

  (global-set-key (kbd "<f10> f") 'my/logos-focus-editing-toggle)

  (global-set-key (kbd "<f10> p") 'org-pomodoro)

  (global-set-key (kbd "<f10> i") 'org-clock-in)
  (global-set-key (kbd "<f10> I") 'org-clock-in-last)

  ;; ;;;###autoload
  ;; (defun my/org-clock-goto (&optional _)
  ;;   (interactive)
  ;;   (let ((buf (get-buffer "org-clock-in-now")))
  ;;     (tab-bar-switch-to-tab "time")
  ;;     (if buf
  ;;         (progn (switch-to-buffer buf)
  ;;                ;; (delete-other-windows)
  ;;                )
  ;;       (org-clock-goto)
  ;;       (rename-buffer "org-clock-in-now")
  ;;       ))

;;;; shift

  ;; Shift 거슬리는 것을 막아주는 아주 요긴한 설정이다.
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-support-shift-select nil) ; default nil
  (setq shift-select-mode nil) ; default t

;;;; performance latex-and-related off

  (setq org-highlight-latex-and-related nil) ;; '(latex script entities))
  (setq org-latex-and-related-regexp nil) ; default nil

;;;; imenu ellipsis bookmark

  ;; Search on https://www.compart.com/en/unicode/U+25BF
  ;; Unicode Character “◉” (U+25C9)
  (setq org-capture-bookmark nil)
  (setq org-ellipsis " ↴") ;; ⚡, ◉, ▼, ↴,

  ;; (setq org-imenu-depth 3) ; default 2

;;;; pretty-entities / bullet lists / image-width

  (setq org-image-actual-width t)
  (setq org-image-max-width (min (/ (display-pixel-width) 3) 640))

  ;; Org styling, hide markup etc. 테스트
  ;; 왜 minemacs 는 org-pretty 설정을 둘다 t 로 했을까?  org-pretty-entities 가
  ;; 설정되면 abc_def 에서 def 가 아래로 기어 들어간다.
  ;; 2023-10-13: I prefer using M-x org-toggle-pretty-entities instead.
  (setq org-pretty-entities nil) ; very important
  ;; orgmode 익스포트 할 때, underscore 가 subscripts 변환 방지
  ;; http://ohyecloudy.com/emacsian/2019/01/12/org-export-with-sub-superscripts/
  (setq org-pretty-entities-include-sub-superscripts nil)

  ;; Use utf-8 bullets for bullet lists -- this isn't great, but a bit
  ;; nicer than nothing. Ideally should use monospace font for spaces
  ;; before bullet item, and use different bullets by list level.
  ;; 2024-05-02 replaced by org-modern
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([+]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

;;;; org-startup-folded

  ;; fold / overview  - collapse everything, show only level 1 headlines
  ;; content          - show only headlines
  ;; nofold / showall - expand all headlines except the ones with :archive:
  ;;                    tag and property drawers
  ;; showeverything   - same as above but without exceptions
  ;; #+startup: fold
  ;; (setq org-startup-folded 'fold) ; show2levels
  (setq org-agenda-inhibit-startup t) ;; ~50x speedup, doom default t

;;;; org-export

  ;; ;; (setq org-export-preserve-breaks t) ; default nil
  ;; ;; (setq org-export-with-properties t) ; default nil
  ;; ;; (setq org-export-use-babel nil) ; default t
  ;; ;; (setq org-export-with-broken-links t) ; default nil

  (setq org-export-with-smart-quotes nil) ; default nil, doom t

  (setq org-export-headline-levels 5) ; default 3
  (setq org-publish-use-timestamps-flag t) ; default t
  (setq org-export-with-section-numbers nil) ; default t
  (setq org-export-with-toc nil) ; default t - turn off on hugo toc
  (setq org-export-with-timestamps nil)

  (setq org-export-with-drawers nil) ; default (not "LOGBOOK")

  (setq org-export-with-todo-keywords nil) ; t
  (setq org-export-with-broken-links t) ; nil
  (setq org-export-with-toc nil)
  (setq org-export-date-timestamp-format "%e %B %Y")

  (setq org-export-with-tags 'not-in-toc)

;;;; org-pomodoro

  (require 'org-pomodoro)
  (setq org-pomodoro-manual-break t)
  (setq org-pomodoro-format "⌛ %s")

  (defun ash/org-pomodoro-til-meeting ()
    "Run a pomodoro until the next 30 minute boundary."
    (interactive)
    (let ((org-pomodoro-length (mod (- 30 (cadr (decode-time (current-time)))) 30)))
      (org-pomodoro)))

  ;; from gopar
  ;; (org-pomodoro-started . gopar/load-window-config-and-close-home-agenda)
  ;; (org-pomodoro-finished . gopar/save-window-config-and-show-home-agenda))
  ;; (defun gopar/home-pomodoro ()
  ;;     (interactive)
  ;;     (setq org-pomodoro-length 25
  ;;         org-pomodoro-short-break-length 5))

  ;; (defun gopar/work-pomodoro ()
  ;;     (interactive)
  ;;     (setq org-pomodoro-length 60
  ;;         org-pomodoro-short-break-length 20))

  ;; (defun gopar/save-window-config-and-show-home-agenda ()
  ;;     (interactive)
  ;;     (window-configuration-to-register ?`)
  ;;     (delete-other-windows)
  ;;     (org-save-all-org-buffers)
  ;;     (org-agenda nil "h"))

  ;; (defun gopar/load-window-config-and-close-home-agenda ()
  ;;     (interactive)
  ;;     (org-save-all-org-buffers)
  ;;     (jump-to-register ?`)))

;;;; org-clock-sound

  (setq org-clock-sound (concat user-dotemacs-dir "assets/sounds/meditation_bell.wav"))

  ;; async
  (defun my/play-meditation-bell()
    "Play meditation-bell"
    (interactive)
    (call-process-shell-command "~/.local/bin/play-meditation-bell.sh" nil 0))
  ;; (global-set-key (kbd "C-c j m") 'my/play-meditation-bell)
  (add-hook 'org-clock-in-hook 'my/play-meditation-bell 'append)
  ;; (add-hook 'org-clock-out-hook 'my/play-meditation-bell 'append)
  ;; (add-hook 'org-clock-goto-hook 'my/play-meditation-bell 'append)
  ;; (add-hook 'org-clock-cancel-hook 'my/play-meditation-bell 'append)
  ;; (add-hook 'org-capture-mode-hook 'my/play-meditation-bell 'append)

;;;; org-num

  (require 'org-num)
  (setq org-num-skip-unnumbered t
        org-num-skip-commented t
        org-num-skip-footnotes t
        org-num-max-level 3)
  ;; org-num-skip-tags '("nonum" "noexport") ; doom default
  ;; (add-hook 'org-mode-hook #'org-num-mode)

;;;; org-footnote

  (setq org-footnote-auto-adjust t) ;; renumber footnotes

;;;; org-block and hide leading stars

  ;; Hide ~*~, ~~~ and ~/~ in org text.
  ;; Org styling, hide markup etc. = / ~
  (setq org-hide-emphasis-markers nil)
  (setq org-hide-block-startup nil)
  (setq org-hide-macro-markers nil)

  (setq org-indent-mode-turns-on-hiding-stars nil) ; default t -- MINE
  (setq org-indent-mode-turns-off-org-adapt-indentation t) ; must t, default t

  ;; (setq org-level-color-stars-only nil) ; doom nil
  ;; org-indent-mode 사용하면 org-hide-leading-stars 자동 on
  (setq org-hide-leading-stars nil) ; doom t

;;;; Indentation

  (setq org-adapt-indentation t)
  (setq org-startup-indented nil) ; doom t
  (setq org-src-preserve-indentation nil) ; doom t
  ;; (setq org-edit-src-content-indentation 0) ; MINE

  ;; Reduce org-indent-indentation-per-level from 2 to 1.
  ;; This keeps =org-tags-column= the same for all headings.
  ;; Avoid inconsistency when eidting outside Emacs, like Orgzly and Beorg.
  ;; (setq org-indent-indentation-per-level 1) ; 2024-06-19 enable, 2024-06-27 turn-off

;;;; defer-font-lock

  (defun locally-defer-font-lock ()
    "Set jit-lock defer and stealth, when buffer is over a certain size."
    (when (> (buffer-size) 50000) ; 50kb
      (setq-local jit-lock-defer-time 0.1 ;; 0.2
                  jit-lock-stealth-time 1)))
  (add-hook 'org-mode-hook #'locally-defer-font-lock)

;;;; org-blank-before-new-entry : heading and plain-list

  ;; 순서 없는 목록(unordered list)에서 bullet으로 들여쓰기를 할 때마다 +, -를 번갈아 사용한다
  (setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))

  (setq org-blank-before-new-entry
        '((heading . t) (plain-list-item . nil)))

  ;; /ohyecloudy-dot-doom/doom.d/config.org

  ;; =M-RET= 키로 라인을 분리할 수 있게 한다. org module에서 nil 값을 바인딩한 걸 디폴트 값으로 돌림.
  (setq org-M-RET-may-split-line '((default . t))) ; doom nil

;;;; org-table

  ;; /vedang-dotfiles-clj-agenda/org-mode-crate/org-mode-crate.el:942:
  (setq org-table-export-default-format "orgtbl-to-csv")

;;;; TODO org-columns

  ;; vedang's style from org-mode-crate
  (setq org-columns-default-format
        "%50ITEM(Task) %5Effort(Effort){:} %5CLOCKSUM %3PRIORITY %20DEADLINE %20SCHEDULED %20TIMESTAMP %TODO %CATEGORY(Category) %TAGS")

;;;; Disable org-element-cache

  ;; 2024-06-27 안쓰는게 나은듯

  ;; The new org-data element provides properties from top-level property drawer,
  ;; buffer-global category, and :path property containing file path for file Org buffers.
  (setq org-element-use-cache nil) ; default t

  ;; Element cache persists across Emacs sessions
  (setq org-element-cache-persistent nil) ; default t

;;;; keybindings - org-mode-map

  (progn
    (define-key org-mode-map (kbd "<f3>") 'org-toggle-link-display)
    (define-key org-mode-map (kbd "<f4>") 'org-toggle-inline-images)

    (define-key org-mode-map (kbd "S-<tab>") (lambda () (interactive) (org-cycle 'FOLDED)))
    (define-key org-mode-map (kbd "S-TAB") (lambda () (interactive) (org-cycle 'FOLDED)))
    (define-key org-mode-map (kbd "<backtab>") (lambda () (interactive) (org-cycle 'FOLDED)))
    (define-key org-mode-map (kbd "S-<iso-lefttab>") (lambda () (interactive) (org-cycle 'FOLDED)))
    (define-key org-mode-map (kbd "C-M-<tab>") 'org-shifttab)

    (define-key org-mode-map (kbd "C-c 1") 'org-show-level-1)
    (define-key org-mode-map (kbd "C-c 2") 'org-show-level-2)
    (define-key org-mode-map (kbd "C-c 3") 'org-show-level-3)
    (define-key org-mode-map (kbd "C-c 4") 'org-show-level-4)

    (define-key org-mode-map (kbd "C-c H") 'org-insert-heading)
    (define-key org-mode-map (kbd "C-c S") 'org-insert-subheading)

    (define-key org-mode-map (kbd "C-c r") #'my/org-random-heading)

    (evil-define-key '(normal visual) org-mode-map (kbd "C-n") 'org-next-visible-heading)
    (evil-define-key '(normal visual) org-mode-map (kbd "C-p") 'org-previous-visible-heading)

    ;; evil-collection
    (evil-define-key '(normal visual) org-mode-map (kbd "C-j") 'org-forward-heading-same-level)
    (evil-define-key '(normal visual) org-mode-map (kbd "C-k") 'org-backward-heading-same-level)

    (evil-define-key '(normal visual) org-mode-map (kbd "C-S-p") 'outline-up-heading)

    (evil-define-key '(normal visual) org-mode-map "zu" 'outline-up-heading)

    (evil-define-key '(insert) org-mode-map (kbd "C-n") 'next-line)
    (evil-define-key '(insert) org-mode-map (kbd "C-p") 'previous-line)

    ;; (evil-define-key '(insert) org-mode-map (kbd "M-h") 'delete-backward-char)
    ;; (evil-define-key '(insert) org-mode-map (kbd "M-l") 'delete-forward-char)

    ;; ordered/unordered list 를 입력 할 때 편함.
    ;; 체크박스가 있는 경우 M-S-RET org-insert-todo-heading 을 활용.
    ;; (evil-define-key '(normal insert visual) org-mode-map (kbd "C-M-<return>") 'org-insert-item)

    ;; 문단을 한 라인으로 합쳐 준다. 구글 번역기 돌릴 때 매우 유용.
    ;; (evil-define-key '(normal insert visual) org-mode-map (kbd "C-M-q") 'unfill-paragraph)

    ;; 복사한 링크는 아래의 방법으로 넣는다. 깔끔해서 좋다.
    ;; org-cliplink 는 insert 니까 i 를 바인딩한다. org-insert-link 를 따른다.
    (evil-define-key '(normal insert visual) org-mode-map (kbd "C-c M-i") 'org-cliplink)
    ;; (define-key map (kbd "C-c M-i") 'org-cliplink)

    (evil-define-key '(insert) org-mode-map (kbd "C-u") 'undo-fu-only-undo)
    (evil-define-key '(insert) org-mode-map (kbd "C-r") 'undo-fu-only-redo)

    ;; flameshot 으로 스크린샷 한 뒤, 바로 붙여넣기
    ;; 22/10/04--15:18 :: flameshot 저장하면 자동으로 클립보드에
    ;; full-path 가 복사된다. imglink 스니펫을 부르고 경로를 복사한다.
    ;; 스크린샷 및 이미지를 관리하기에 이러한 방법이 더 좋다.
    ;; (evil-define-key '(normal insert visual) org-mode-map (kbd "C-c M-y") 'org-download-clipboard)
    (define-key org-mode-map (kbd "C-c M-y") 'org-download-clipboard)

    ;; ;; search to narrow with heading and tag
    (define-key org-mode-map (kbd "C-c o") 'consult-org-heading) ;; GOOD

    (define-key prog-mode-map (kbd "C-M-y") 'evil-yank)

    (define-key org-mode-map (kbd "C-c y") 'org-cliplink)
    (define-key org-mode-map (kbd "C-c I") 'org-insert-link-dwim) ; org-insert-link

    ;; C-x x
    (define-key ctl-x-x-map "h" #'prot-org-id-headline) ; C-x x h
    (define-key ctl-x-x-map "H" #'prot-org-id-headlines)
    ;; (define-key ctl-x-x-map "e" #'prot-org-ox-html)
    (define-key org-mode-map (kbd "C-x x C") 'org-clone-subtree-with-time-shift)

    ;; Shortcuts to Interactive Functions
    (global-set-key (kbd "C-x n m") #'my/split-and-indirect-orgtree)
    (global-set-key (kbd "C-x n M") #'my/kill-and-unsplit-orgtree)
    )

;;;; org-todo-keywords

  (message "Press `C-c a' to get started with your agenda...")

  ;; https://whhone.com/emacs-config/
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "|" "DONE(d)" "CANCELLED(c)")))

  (with-no-warnings
    (custom-declare-face '+org-todo-todo  '((t (:inherit (bold error org-todo)))) "")
    (custom-declare-face '+org-todo-next  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-done  '((t (:inherit (bold success org-todo)))) "")
    (custom-declare-face '+org-todo-prog  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-cancelled '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    )

  (setq org-todo-keyword-faces
        '(("TODO" . +org-todo-todo) ;; red
          ("DONE" . +org-todo-done) ;; green
          ("NEXT" . +org-todo-next) ;; yellow
          ("PROG" . +org-todo-prog) ; blue
          ("CANCELLED" . +org-todo-cancelled) ;; green
          ))

  ;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
  (setq org-agenda-custom-commands
        '(("n" "Agenda / PROG / NEXT"
           ((agenda "" nil)
            (tags "INBOX+LEVEL=2|CATEGORY=\"Inbox\"+LEVEL=1")
            (todo "PROG" nil)
            (todo "NEXT" nil)
            ;; (todo "TODO" nil) ;; 2024-03-18 add
            ) nil)
          (" " "Agenda and all TODOs" ; default' view
           ((agenda "")
            (alltodo "")))))

;;;; org-contacts/agenda files

  (if (locate-library "org-contacts")
      (require 'org-contacts))

  (setq org-contacts-files (list (my/org-contacts-file)))

  ;; My agenda files. keep it simple
  (setq org-user-agenda-files (list
                               (my/org-inbox-file)
                               (my/org-tasks-file)
                               ))
  (setq org-agenda-files org-user-agenda-files)

  (setq org-agenda-diary-file (my/org-diary-file))
  (setq org-default-notes-file (my/org-inbox-file))

  ;; doom-emacs capture files : absolute path
  (setq +org-capture-todo-file (my/org-inbox-file))
  (setq +org-capture-projects-file (my/org-tasks-file))
  (setq +org-capture-notes-file (my/org-inbox-file))
  (setq +org-capture-changelog-file (my/org-inbox-file))
  (setq +org-capture-journal-file (my/org-diary-file))

;;;; org-agenda

  ;; Use sticky agenda since I need different agenda views (personal and work) at the same time.
  (setq org-agenda-sticky t) ; default nil

  ;; Shift the agenda to show the previous 3 days and the next 7 days for
  ;; better context on your week. The past is less important than the future.
  (setq org-agenda-span 'day) ; default 'week, doom 10

  ;; Hide all scheduled todo.
  (setq org-agenda-todo-ignore-scheduled 'all)

  ;; Ignores "far" deadline TODO items from TODO list.
  (setq org-agenda-todo-ignore-deadlines 'far)

  ;; Hide all scheduled todo, from tags search view, like tags-todo.
  (setq org-agenda-tags-todo-honor-ignore-options t)

  ;; Hide all done todo in agenda
  (setq org-agenda-skip-scheduled-if-done t)

  ;; Hide task until the scheduled date.
  (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)

  (setq org-log-into-drawer t)

  (setq org-log-done 'time)

  ;; (setcdr (assoc 'note org-log-note-headings) "%d")
  ;; Interstitial Journaling: add note to CLOCK entry after clocking out
  ;; https://emacs.stackexchange.com/questions/37526/add-note-to-clock-entry-after-clocking-out
  (setq org-log-note-clock-out t)

  ;; 4 priorities to model Eisenhower's matrix.
  ;; - [#A] means +important +urgent
  ;; - [#B] means +important -urgent
  ;; - [#C] means -important +urgent
  ;; - [#D] means -important -urgent
  (setq org-priority-default 68
        org-priority-lowest 68)

;;;; diary-file

  (setq diary-file (concat user-dotemacs-dir "diary"))
  (setq org-agenda-include-diary t)

;;;; org-agenda-log-mode and clock-mode

  ;; Show all agenda dates - even if they are empty
  (setq org-agenda-show-all-dates t)
  (setq org-agenda-start-with-log-mode t)

  ;; Agenda log mode items to display (closed clock : default)
  ;; 이전 이맥스는 state 가 기본이었다. 지금은 시간 기준으로 표기한다.
  ;; closed    Show entries that have been closed on that day.
  ;; clock     Show entries that have received clocked time on that day.
  ;; state     Show all logged state changes.
  ;; (setq org-agenda-log-mode-items '(closed clock state))
  (setq org-agenda-log-mode-add-notes nil)

  ;; sort 관련 기능을 확인해보고 정의한 함수들이 필요 없으면 빼면 된다.
  (setq org-agenda-sort-notime-is-late t) ; Org 9.4
  (setq org-agenda-sort-noeffort-is-high t) ; Org 9.4

  ;; Time Clocking
  (setq org-clock-idle-time 30) ; 10
  (setq org-clock-reminder-timer (run-with-timer
                                  t (* org-clock-idle-time 20) ; 60
                                  (lambda ()
                                    (unless (org-clocking-p)
                                      (alert "Do you forget to clock-in?"
                                             :title "Org Clock")))))
  ;; (org-clock-auto-clockout-insinuate) ; auto-clockout
  ;; modeline 에 보이는 org clock 정보가 너무 길어서 줄임
  (setq org-clock-string-limit 30) ; default 0

  ;; org-clock-persist for share with machines
  (setq org-clock-persist-query-save t)
  (setq org-clock-persist-query-resume t)

  ;; current  Only the time in the current instance of the clock
  ;; today    All time clocked into this task today
  ;; repeat   All time clocked into this task since last repeat
  ;; all      All time ever recorded for this task
  ;; auto     Automatically, either all, or repeat for repeating tasks
  (setq org-clock-mode-line-entry t)
  (setq org-clock-mode-line-line-total 'auto) ; default nil

  ;; global Effort estimate values
  ;; global STYLE property values for completion
  (setq org-global-properties
        (quote
         (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 8:00")
          ("STYLE_ALL" . "habit"))))

  ;; Include the file name into the path in refile target.
  ;; (setq org-refile-use-outline-path 'file) ; default nil
  ;; (setq org-outline-path-complete-in-steps nil) ; default t

  ;; (setq org-refile-targets
  ;;       `((nil :maxlevel . 2)
  ;;         (,(my/org-tasks-file) :maxlevel . 2)
  ;;         (,(my/org-links-file) :maxlevel . 2)))

  ;; Save Org buffers after refiling!
  ;; (advice-add 'org-refile :after 'org-save-all-org-buffers)

;;;; consult-org-agenda

  ;; (defun my/consult-org-agenda ()
  ;;   (interactive)
  ;;   (consult-org-agenda)
  ;;   (org-tree-to-indirect-buffer))

  (setq my/consult-org-files '())
  (add-to-list 'my/consult-org-files (my/org-inbox-file) t)
  (add-to-list 'my/consult-org-files (my/org-tasks-file) t)
  (add-to-list 'my/consult-org-files (my/org-links-file) t)
  (add-to-list 'my/consult-org-files (my/org-contacts-file) t)
  (add-to-list 'my/consult-org-files (my/org-mobile-file) t)
  ;; (add-to-list 'my/consult-org-files (my/org-diary-file) t)

  (when (file-exists-p (my/org-blog-file))
    (add-to-list 'my/consult-org-files (my/org-blog-file) t))

  (when (file-exists-p (my/org-reading-file))
    (add-to-list 'my/consult-org-files (my/org-reading-file) t))

  ;; (when (file-exists-p (my/org-emacs-config-file))
  ;;     (add-to-list 'my/consult-org-files (my/org-emacs-config-file) t))

  (defun my/consult-org-all ()
    (interactive)
    (consult-org-heading
     "+LEVEL<=2" ; 3
     my/consult-org-files))

  (defun my/consult-org-contacts ()
    (interactive)
    (consult-org-heading
     "+LEVEL<=3"
     (list (my/org-contacts-file))))

  (defun my/consult-org-inbox ()
    (interactive)
    (consult-org-heading
     "+LEVEL<=3"
     (list (my/org-inbox-file))))

  (defun my/consult-org-tasks ()
    (interactive)
    (consult-org-heading
     "+LEVEL<=3"
     (list (my/org-tasks-file))))

  (defun my/consult-org-links ()
    (interactive)
    (consult-org-heading
     "+LEVEL<=3"
     (list (my/org-links-file))))

  (defun my/consult-org-quote ()
    (interactive)
    (consult-org-heading
     "+LEVEL<=3"
     (list (my/org-quote-file))))

  (defun my/consult-org-kdc ()
    (interactive)
    (consult-org-heading
     "+LEVEL<=3"
     (list (my/org-kdc-file))))

  (defun my/consult-org-blog ()
    (interactive)
    (consult-org-heading
     "+LEVEL<=3"
     (list (my/org-blog-file))))

  (defun my/consult-org-reading ()
    (interactive)
    (consult-org-heading
     "+LEVEL<=3"
     (list (my/org-reading-file))))

;;;; org-tag and category

  ;; (setq org-auto-align-tags nil) ; default t, use doom's custom
  ;; (setq org-tags-column 0) ; default -77
  (setq org-agenda-tags-column -80) ;; 'auto ; org-tags-column
  (setq org-agenda-show-inherited-tags nil)

  (setq org-tag-alist (quote (
                              (:startgroup) ;; Location
                              ("@Errand" . ?E)
                              ("@Office" . ?O)
                              ("@Library" . ?L)
                              ("@Cafe" . ?C)
                              ("@Home" . ?H)
                              (:endgroup)
                              (:startgroup) ;; Action
                              ("@Read" . ?R)
                              ("@Write" . ?W)
                              (:endgroup)
                              (:startgroup) ;; Category
                              ("@Personal" . ?P)
                              ("@Family" . ?F)
                              (:endgroup) ;; Status
                              ("WAITING" . ?w)
                              ("IMPORTANT" . ?i)
                              ("NEXT" . ?n)
                              ("HOLD" . ?h)
                              ("CANCELLED" . ?c)
                              ("crypt" . ?E)
                              ("NOTE" . ?n)
                              ("noexport" . ?x)
                              ("nonum" . ?u)
                              ("LATEST" . ?l) ;; latest version
                              ("FLAGGED" . ??))))

  (add-to-list 'org-tags-exclude-from-inheritance "projects")

;;;; org-journal/agenda

  ;; (add-hook 'org-agenda-mode-hook
  ;;           (lambda ()
  ;;             (calendar-set-date-style 'iso)))

  (defun org-journal-new-entry ()
    "Inserts header with inactive timestamp, hours and minutes.
     A custom journal helper function."
    (interactive)
    (org-insert-heading)
    (org-insert-time-stamp (current-time) t t))

  ;; Clock break time in pomodoro
  (setq org-pomodoro-clock-break t)

  ;; Get a timestamp for tomorrow
  (defun my/tomorrow ()
    (format-time-string "%Y-%m-%d" (time-add 86400 (current-time))))

;;;; src-lang-modes

  ;; (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))

;;;; edit-src-code

  ;; Disable editing source code in dedicated buffer
  ;; https://emacs.stackexchange.com/questions/73986/how-do-i-stop-org-babel-from-trying-to-edit-a-source-block-in-a-dedicated-buffer/73988#73988
  ;; (defun org-edit-src-code nil)

;;;; link-abbrev

  ;; 추가
  (add-to-list 'org-link-abbrev-alist
               '("wikidata"        . "https://www.wikidata.org/wiki/"))

;;;; procratinate

  (defun org-procrastinate ()
    "Set the scheduled date on an Org agenda item to tomorrow."
    (interactive)
    (org-agenda-schedule nil "+1d"))

;;;; org-agenda-custom-commands

  ;; ol-doi ol-w3m ol-bbdb ol-docview ol-gnus ol-info ol-irc ol-mhe ol-rmail
  ;; ol-eww ol-bibtex
  ;; Adapted from http://stackoverflow.com/a/12751732/584121
  ;; (require 'org-protocol)
  (setq org-protocol-default-template-key "L")
  (setq org-modules `(
                      org-habit
                      org-protocol
                      ))

  ;; (setq org-agenda-prefix-format
  ;;       '((agenda  . " %i %-14:c%?-12t% s")
  ;;         (todo  . " %i %-14:c")
  ;;         (tags  . " %i %-14:c")
  ;;         (search . " %i %-14:c")))

  ;; https://www.pygopar.com/creating-new-columns-in-org-agenda
  ;; Originally from here: https://stackoverflow.com/a/59001859/2178312
  (defun gopar/get-schedule-or-deadline-if-available ()
    (let ((scheduled (org-get-scheduled-time (point)))
          (deadline (org-get-deadline-time (point))))
      (if (not (or scheduled deadline))
          (format " ")
        ;; (format "🗓️ ")
        "   ")))

  (setq org-agenda-prefix-format
        '((agenda . " %-4e %i %-12:c%?-12t% s ")
          (todo . " %i %-10:c %-5e %(gopar/get-schedule-or-deadline-if-available)")
          (tags . " %i %-12:c")
          (search . " %i %-12:c")))

  (when IS-TERMUX
    (setq org-agenda-prefix-format
          '((agenda  . " %i %?-12t% s")
            (todo  . " %i ")
            (tags  . " %i ")
            (search . " %i "))))

  (setq org-agenda-category-icon-alist nil)

  (setq org-agenda-hide-tags-regexp
        "agenda\\|CANCELLED\\|LOG\\|ATTACH\\|GENERAL\\|BIRTHDAY\\|PERSONAL\\|PROFESSIONAL\\|TRAVEL\\|PEOPLE\\|HOME\\|FINANCE\\|PURCHASES")

  (add-hook 'org-agenda-finalize-hook
            (lambda ()
              ;; (setq-local line-spacing 0.2)
              (define-key org-agenda-mode-map
                          [(double-mouse-1)] 'org-agenda-goto-mouse)))

  (defun cc/org-agenda-goto-now ()
    "Redo agenda view and move point to current time '← now'"
    (interactive)
    (org-agenda-redo)
    (org-agenda-goto-today)

    (if window-system
        (search-forward "← now ─")
      (search-forward "now -"))
    )

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (define-key org-agenda-mode-map (kbd "<f2>") 'org-save-all-org-buffers)
              (define-key org-agenda-mode-map (kbd "M-p") 'org-pomodoro)
              (define-key org-agenda-mode-map (kbd "M-P") 'ash/org-pomodoro-til-meeting)
              (define-key org-agenda-mode-map (kbd "M-.") 'cc/org-agenda-goto-now)))

  (add-hook 'evil-org-agenda-mode-hook
            (lambda ()
              ;; (evil-define-key 'motion evil-org-agenda-mode-map "gt" 'centaur-tabs-forward) ; default doom's bindings
              ;; (evil-define-key 'motion evil-org-agenda-mode-map "gT" 'centaur-tabs-backward)
              (evil-define-key 'motion evil-org-agenda-mode-map "gt" 'tab-line-switch-to-next-tab) ; default doom's bindings
              (evil-define-key 'motion evil-org-agenda-mode-map "gT" 'tab-line-switch-to-prev-tab)
              (evil-define-key 'motion evil-org-agenda-mode-map "F" 'org-agenda-follow-mode)))

  ;; (setq org-archive-location "archives/%s_archive::")
  (setq org-archive-location (file-name-concat org-directory "archives/%s::"))

  ;; nil 이면 C-c C-o 으로 접근한다.
  ;; (setq org-mouse-1-follows-link t) ; default 450

;;;; Default Capture Templates (from Doom-Emacs)

  (setq org-capture-template-dir (concat user-dotemacs-dir "captures/"))
  (setq org-datetree-add-timestamp t)

  (setq org-capture-templates
        '(
          ;; ("t" "todo" entry (file+headline +org-capture-todo-file "Inbox")
          ;;  "* [ ] %?\n%i\n%a")

          ;; ("d" "deadline" entry (file+headline +org-capture-todo-file "Inbox")
          ;;  "* [ ] %?\nDEADLINE: <%(org-read-date)>\n\n%i\n%a")

          ;; ("s" "schedule" entry (file+headline +org-capture-todo-file "Inbox")
          ;;  "* [ ] %?\nSCHEDULED: <%(org-read-date)>\n\n%i\n%a")

          ;; ("T" "Todo ⏰" entry (file +org-capture-todo-file)
          ;;  "* TODO [#C] %?\n%T\n%a\n" :clock-in t :clock-resume t)

          ;; ("n" "Notes" entry (file+headline +org-capture-notes-file "Notes")
          ;;  "* %u %?\n%i\n%a")

          ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file)
           "* %<%H:%M> - %?\n%T\n%a\n")
          ;; "* %U %?\n%i\n%a" :prepend t)
          ("J" "Journal ⏰" entry (file+olp+datetree +org-capture-journal-file)
           "* %<%H:%M> - %?\n%U\n%a\n" :clock-in t :clock-resume t) ; :tree-type week

          ;; import DOOM Emacs Templates
          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ;; Uses the basename from `+org-capture-todo-file',
          ;; `+org-capture-changelog-file' and `+org-capture-notes-file'.
          ;; ("p" "Templates for projects")
          ;; ("pt" "Project-local todo" entry  ; {project-root}/todo.org
          ;;  (file+headline +org-capture-project-todo-file "Inbox")
          ;;  "* TODO %?\n%i\n%a" :prepend t)
          ;; ("pn" "Project-local notes" entry  ; {project-root}/notes.org
          ;;  (file+headline +org-capture-project-notes-file "Inbox")
          ;;  "* %U %?\n%i\n%a" :prepend t)
          ;; ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
          ;;  (file+headline +org-capture-project-changelog-file "Unreleased")
          ;;  "* %U %?\n%i\n%a" :prepend t)

          ;; Will use {org-directory}/{+org-capture-projects-file} and store
          ;; these under {ProjectName}/{Tasks,Notes,Changelog} headings. They
          ;; support `:parents' to specify what headings to put them under, e.g.
          ;; :parents ("Projects")
          ;; ("o" "Centralized templates for projects")
          ;; ("ot" "Project Todo" entry
          ;;  (function +org-capture-central-project-todo-file)
          ;;  "* TODO %?\n %i\n %a"
          ;;  :heading "Tasks"
          ;;  :prepend nil)
          ;; ("on" "Project Notes" entry
          ;;  (function +org-capture-central-project-notes-file)
          ;;  "* %U %?\n %i\n %a"
          ;;  :heading "Notes"
          ;;  :prepend t)
          ;; ("oc" "Project Changelog" entry
          ;;  (function +org-capture-central-project-changelog-file)
          ;;  "* %U %?\n %i\n %a"
          ;;  :heading "Changelog"
          ;;  :prepend t)
          )
        )

  ;; See https://orgmode.org/manual/Template-elements.html#index-org_002ddefault_002dnotes_002dfile-1
  ;; (setq org-capture-templates nil)
  (add-to-list
   'org-capture-templates
   `("i" "Inbox" entry (file+headline ,(my/org-inbox-file) "Inbox")
     "* %?\n%i\n%a"))

  (add-to-list
   'org-capture-templates
   `("I" "Inbox (Work)" entry (file+headline ,(my/org-inbox-file) "Inbox")
     "* %? :work:\n%i\n%a"))

  (add-to-list
   'org-capture-templates
   `("p" "Project /w template" entry (file+headline ,(my/org-tasks-file) "Projects")
     (file ,(concat org-capture-template-dir "project.capture"))))

  (add-to-list
   'org-capture-templates
   `("v" "Vocab" entry (file+headline ,(my/org-drill-file) "Translation")
     "* %? :drill:\n\n** Translation\n\n** Definition\n"))

  (unless IS-TERMUX
    (add-to-list
     'org-capture-templates
     `("c" "Contacts" entry (file ,(my/org-contacts-file))
       "* %(org-contacts-template-name)\n:PROPERTIES:\n:ID: %(org-id-uuid)\n:GITHUB:\n:EMAIL: a@a.com\n:URL:\n:NOTE:\n:END:\n%U\n%T\n%a\n"))

    (add-to-list
     'org-capture-templates
     `("l" "links" entry (file ,(my/org-links-file))
       "* TODO %(org-cliplink-capture)" :immediate-finish t))
    )

  ;; (add-to-list
  ;;  'org-capture-templates
  ;;  `("T" "Personal Todo /w clock-in" entry (file ,(my/org-inbox-file))
  ;;    "* TODO [#C] %?\n%T\n%a\n" :clock-in t :clock-resume t))

  ;; (push `("j" "journal entry" entry (file+olp+datetree ,(my/org-diary-file))
  ;;         "* %<%H:%M> - %?\n%T\n")
  ;;       org-capture-templates)
  ;; (push `("J" "Journal /w clock-in" entry (file+olp+datetree ,(my/org-diary-file))
  ;;         "* %<%H:%M> - %?\n%U\n%a\n" :clock-in t :clock-resume t :empty-lines 1)
  ;;       org-capture-templates)

;;;; org-id-location

  (setq org-id-locations-file (file-name-concat org-directory (concat "." system-name "-orgids"))) ;; share

;;;; org-insert-heading-respect-content

  ;; overide here! important
  (setq org-insert-heading-respect-content nil) ; doom t

;;;; org-link-set-parameters

  (progn
    ;; 2024-06-04 file - id - http/https
    (org-link-set-parameters "file" :face `(:inherit link :weight bold :slant italic :underline t)) ;; italic
    (org-link-set-parameters "id" :face `(:inherit success :weight bold :underline t))
    (org-link-set-parameters "http" :face `(:inherit warning :weight semibold :underline t))
    (org-link-set-parameters "info" :face `(:inherit info-file :weight semibold :underline t))
    (org-link-set-parameters "https" :face `(:inherit warning :weight semibold :underline t))
    )
;;;; org-cliplink

  (with-eval-after-load 'org-cliplink
    (require 'org-cliplink)
    (setq org-cliplink-max-length 72)
    (setq org-cliplink-ellipsis "-")

    ;; from ohyecloudy
    (defun my/org-cliplink ()
      (interactive)
      (org-cliplink-insert-transformed-title
       (org-cliplink-clipboard-content)     ;take the URL from the CLIPBOARD
       #'my-org-link-transformer))

    (defun my-org-link-transformer (url title)
      (let* ((parsed-url (url-generic-parse-url url)) ;parse the url
             (host-url (replace-regexp-in-string "^www\\." "" (url-host parsed-url)))
             (clean-title
              (cond
               ;; if the host is github.com, cleanup the title
               ((string= (url-host parsed-url) "github.com")
                (replace-regexp-in-string "^/" ""
                                          (car (url-path-and-query parsed-url))))
               ;; otherwise keep the original title
               (t (my-org-cliplink--cleansing-site-title title))))
             (title-with-url (format "%s - %s" clean-title host-url)))
        ;; forward the title to the default org-cliplink transformer
        (org-cliplink-org-mode-link-transformer url title-with-url)))

    (defun my-org-cliplink--cleansing-site-title (title)
      (let ((result title)
            (target-site-titles '(" - 위키백과"
                                  " - Wikipedia"
                                  " - PUBLY"
                                  " - YES24"
                                  "알라딘: "
                                  " : 클리앙"
                                  " - YouTube")))
        (dolist (elem target-site-titles)
          (if (string-match elem result)
              (setq result (string-replace elem "" result))
            result))
        result))

    ;; 마지막에 host 를 붙이고 싶어서 link transformer 함수를 짰다. =title -
    ;; ohyecloudy.com= 식으로 org link 를 만든다.
    (define-key org-mode-map [remap org-cliplink] 'my/org-cliplink)
    )

;;;; org-elfeed

  (with-eval-after-load 'elfeed
    (setq rmh-elfeed-org-files (concat user-org-directory "elfeed.org"))
    (setq elfeed-search-filter "@6-months-ago") ;;  "@1-month-ago +unread"
    (setq elfeed-search-title-max-width 80) ; default 70
    (add-hook 'elfeed-search-mode-hook #'elfeed-update)
    )
;;;; END
  )

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

;;;; casual-suite

(use-package casual-suite
  :defer 1
  :config
  (define-key Info-mode-map (kbd "<f2>") #'casual-info-tmenu)
  (global-set-key (kbd "M-g v") 'casual-avy-tmenu)
  (define-key dired-mode-map (kbd "<f2>") #'casual-dired-tmenu)
  (define-key calc-mode-map (kbd "<f2>") #'casual-calc-tmenu)
  (define-key isearch-mode-map (kbd "<f2>") #'casual-isearch-tmenu)
  (define-key ibuffer-mode-map (kbd "<f2>") #'casual-ibuffer-tmenu)
  )

;;; Easy mode

(unless IS-TERMUX
  ;; context-mode
  (menu-bar-mode 1)
  )

;;; Load Workspaces & Themes

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

(load-theme 'modus-operandi :no-confirm)

;;; init.el ends here
