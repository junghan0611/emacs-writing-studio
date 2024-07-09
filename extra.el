;;; extra.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Junghan Kim
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; Maintainer: Junghan Kim <junghanacs@gmail.com>
;; Created: May 02, 2024
;; Modified: May 02, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/junghan0611/extra
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;;; TODO diff-hl

(use-package diff-hl
  ;; :vc (diff-hl :url "git@github.com:whhone/diff-hl.git" :branch "with-editor-fix")
  :init
  (global-diff-hl-mode)
  :config
  ;; Added in https://github.com/dgutov/diff-hl/pull/207
  (setq diff-hl-update-async t)

  ;; Disable async in `with-editor-mode'.
  ;; https://github.com/dgutov/diff-hl/issues/213
  (add-hook 'find-file-hook
            (lambda ()
              (when (bound-and-true-p with-editor-mode)
                (setq-local diff-hl-update-async nil))))

  (diff-hl-flydiff-mode +1)
  :hook
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

;;; dired

(with-eval-after-load 'dired
  (setq dired-make-directory-clickable t) ; Emacs 29.1, doom t
  (setq dired-free-space nil) ; Emacs 29.1, doom first

  ;; Better dired flags:
  ;; `-l' is mandatory
  ;; `-a' shows all files
  ;; `-h' uses human-readable sizes
  ;; `-F' appends file-type classifiers to file names (for better highlighting)
  ;; -g     like -l, but do not list owner
  (setq dired-listing-switches "-AGFhgv --group-directories-first --time-style=long-iso") ;; doom "-ahl -v --group-directories-first"
  (setq dired-recursive-copies 'always ; doom 'always
        dired-dwim-target t) ; doom t
  (setq dired-ls-F-marks-symlinks t ; doom nil -F marks links with @
        delete-by-moving-to-trash t) ; doom nil

  (setq dired-use-ls-dired t)  ; doom t
  (setq dired-do-revert-buffer t) ; doom nil
  (setq dired-kill-when-opening-new-dired-buffer t) ; doom nil
  (setq dired-clean-confirm-killing-deleted-buffers t) ; doom nil

  (require 'wdired)
  (setq wdired-allow-to-change-permissions t) ; doom nil
  (setq wdired-create-parent-directories t)

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (setq-local truncate-lines t) ; Do not wrap lines
              ;; (visual-line-mode -1)
              (hl-line-mode 1)))
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (remove-hook 'dired-mode-hook 'dired-omit-mode)
  )

;;;; dired-preview

(use-package dired-preview
  :after dired
  :commands dired-preview
  :init
  (setq dired-preview-delay 0.7)
  (setq dired-preview-max-size (expt 2 20)) ;; => 1048576
  (defun my-dired-preview-to-the-right ()
    "My preferred `dired-preview-display-action-alist-function'."
    '((display-buffer-in-side-window)
      (side . right)
      (width . 0.3)))
  ;; default' dired-preview-display-action-alist-dwim
  (setq dired-preview-display-action-alist-function #'my-dired-preview-to-the-right)
  )

;;; Hangul Korean

(setq default-input-method "korean-hangul")
(set-language-environment "Korean")
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system  'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

(set-selection-coding-system 'utf-8) ;; important

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq-default line-spacing 3) ; use fontaine

;; (setenv "LANG" "en_US.UTF-8")
;; (setenv "LC_ALL" "en_US.UTF-8")
;; (setenv "LANG" "ko_KR.UTF-8")

;; 날짜 표시를 영어로한다. org mode에서 time stamp 날짜에 영향을 준다.
(setq system-time-locale "C")

(setq input-method-verbose-flag nil
      input-method-highlight-flag nil)

(global-set-key (kbd "<S-SPC>") 'toggle-input-method)
;; (global-set-key (kbd "<Alt_R>") 'toggle-input-method)
(global-set-key (kbd "<Hangul>") 'toggle-input-method)
;; (global-unset-key (kbd "S-SPC"))

;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; +------------+------------+
;; | ABCDEFGHIJ | ABCDEFGHIJ |
;; +------------+------------+
;; | 1234567890 | 1234567890 |
;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; | abcdefghij | abcdefghij |
;; +------------+------------+


(when (display-graphic-p)
  (set-face-attribute 'default nil :family "Monoplex KR Nerd" :width 'normal :weight 'regular :height 140)
  (set-fontset-font nil 'hangul (font-spec :family "Monoplex KR Nerd"))
  ;; (set-face-attribute 'fixed-pitch nil :family "Sarasa Term K" :width 'normal :weight 'regular)
  ;; (set-face-attribute 'fixed-pitch-serif nil :family "Hahmlet" :width 'normal :weight 'regular)
  ;; (set-face-attribute 'variable-pitch nil :family "Pretendard Variable"
  ;;                     :width 'normal :weight 'regular)

  (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil)
  (set-fontset-font t 'emoji (font-spec :family "Noto Emoji") nil 'prepend) ; Top

  (set-fontset-font t 'symbol (font-spec :family "Symbola") nil)
  (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols 2") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols") nil 'prepend)
  )

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun load-theme@run-hooks (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :after #'load-theme@run-hooks)

(defun load-theme@theme-dont-propagate (&rest _)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))
(advice-add #'load-theme :before #'load-theme@theme-dont-propagate)

;;; ox-hugo

(use-package ox-hugo
  :ensure t
  :after org
  :config
  ;; (setq org-hugo-base-dir (file-truename "~/git/blog/"))
  (setq org-hugo-base-dir "~/git/blog/")

  ;; (setq org-hugo-auto-set-lastmod t
  ;;       org-hugo-suppress-lastmod-period 43200.0)

  (setq org-hugo-front-matter-format 'yaml)

  (setq org-hugo-section "blog") ; 2024-04-26 change
  (setq org-hugo-paired-shortcodes "mermaid callout cards details tabs") ; hint sidenote

  ;; https://ox-hugo.scripter.co/doc/formatting/
  ;; if org-hugo-use-code-for-kbd is non-nil
  ;; Requires CSS to render the <kbd> tag as something special.
  ;; eg: ~kbd~
  ;; (setq org-hugo-use-code-for-kbd t)

  ;; https://ox-hugo.scripter.co/doc/linking-numbered-elements/

  ;; org-export-dictionary 에 Figure, Table 에 한글 번역을 넣으면
  ;; 한글로 바뀌어 export 될 것이다.
  (setq org-hugo-link-desc-insert-type t)

  ;; 내보낼 때는 fill-column 끈다.
  (setq org-hugo-preserve-filling nil) ; important

  (setq org-hugo-allow-spaces-in-tags t) ; default t
  (setq org-hugo-prefer-hyphen-in-tags t) ; default t

  ;; Assume all static files are images for now otherwise this
  ;; defaults to /ox-hugo/mypicture.png which is ugly
  (setq org-hugo-default-static-subdirectory-for-externals "images") ; imgs

  ;; Override the default `org-hugo-export-creator-string' so that this
  ;; string is consistent in all ox-hugo tests.
  (setq org-hugo-export-creator-string "Emacs + Org mode + ox-hugo")

  ;; In that normal example of the sidenote, ox-hugo trims the whitespace around
  ;; the sidenote block. That is configured by customizing the
  ;; org-hugo-special-block-type-properties variable:
  (progn
    (add-to-list 'org-hugo-special-block-type-properties '("mermaid" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("callout" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("cards" :raw t))
    (add-to-list 'org-hugo-special-block-type-properties '("details" :raw t))
    )
  ;; (add-to-list 'org-hugo-special-block-type-properties '("sidenote" . (:trim-pre t :trim-post t)))

  ;; If this property is set to an empty string, this heading will not be auto-inserted.
  ;; default value is 'References'
  ;; https://ox-hugo.scripter.co/doc/org-cite-citations/
  (plist-put org-hugo-citations-plist :bibliography-section-heading "References")
  )

;;; nerd-icons

(use-package nerd-icons
  :ensure t
  :commands (nerd-icons-octicon
             nerd-icons-faicon
             nerd-icons-flicon
             nerd-icons-wicon
             nerd-icons-mdicon
             nerd-icons-codicon
             nerd-icons-devicon
             nerd-icons-ipsicon
             nerd-icons-pomicon
             nerd-icons-powerline))
(use-package nerd-icons-dired)
(use-package nerd-icons-completion)

(use-package nerd-icons-corfu
  :after corfu
  :if window-system
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  (add-hook 'dired-mode-hook 'nerd-icons-dired-mode)
  (nerd-icons-completion-mode)
  )

;;; shackle

(use-package shackle
  :init
  (setq shackle-default-size 0.4
        shackle-rules `(
                        ;; (help-mode                       :select t :align right :size ,fill-column)
                        ;; (helpful-mode                    :select t :align right :size ,fill-column)

                        ;; select nil 일 때, 'q' 로 바로 닫을 수 있다.
                        (help-mode                       :select nil :align t)
                        (helpful-mode                    :select nil :align t)

                        ("*Messages*"                    :select nil :align t)
                        ("*eldoc*"                       :align t)
                        (special-mode                    :align t)
                        (process-menu-mode               :align t)
                        (compilation-mode                :align t)
                        (flymake-diagnostics-buffer-mode :align t)
                        ("*Shell Command Output*"        :align t)
                        ("*Async Shell Command*"         :align t)
                        ("\\*EGLOT.*"                    :select t :align right :size ,fill-column :regexp t)))

  (add-hook 'after-init-hook 'shackle-mode)
  )
;;; popper

(use-package popper
  :ensure t
  :init
  (setq popper-echo-dispatch-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p))
  (setq popper-display-control nil) ; use popwin and display-buffer-alist
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          ;; "Output\\*$"
          "*cider-error*"
          ;; "*cider-doc*"
          ;; "^\\*eldoc for"
          "\\*Async-native-compile-log\\*" ; JH
          "^\\*EGLOT" ; JH
          ;; treemacs-mode ; JH
          "*Go-Translate*" ; JH
          "*wordreference*" ; JH
          "*tmr-tabulated-view*" ; JH
          "*SDCV*" ; JH
          "*Dogears List*" ; JH
          "^\\*Backtrace\\*"
          "*Hammy Log*"
          ;; "*eww*"
          "*lsp-documentation*"
          "*devdocs-javascript*"
          ;; zk-index-mode
          help-mode
          telega-chat-mode
          helpful-mode
          compilation-mode
          process-menu-mode
          special-mode
          eww-mode
          ;; "*Emacs Log*"
          ;; "*command-log*" ; JH
          flymake-diagnostics-buffer-mode))
  (add-to-list
   'popper-reference-buffers
   '(("^\\*Warnings\\*$" . hide)
     ("^\\*Compile-Log\\*$" . hide)
     "^\\*Matlab Help.*\\*$"
     "^\\*Messages\\*$"
     ("*typst-ts-compilation*" . hide)
     ("^\\*dash-docs-errors\\*$" . hide)
     "^\\*evil-registers\\*"
     "^\\*Apropos"
     "^Calc:"
     "^\\*eldoc\\*"
     "^\\*TeX errors\\*"
     "^\\*ielm\\*"
     "^\\*TeX Help\\*"
     "^\\*ChatGPT\\*"
     "^\\*gptel-quick\\*"
     "^\\*define-it:"
     "\\*Shell Command Output\\*"
     "\\*marginal notes\\*"
     ("\\*Async Shell Command\\*" . hide)
     "\\*Completions\\*"
     "[Oo]utput\\*"))

  ;; (global-set-key (kbd "C-`") 'popper-toggle)
  ;; (global-set-key (kbd "C-~") 'popper-kill-latest-popup)
  ;; (global-set-key (kbd "M-`") 'popper-cycle)
  ;; (global-set-key (kbd "C-M-`") 'popper-toggle-type)
  (popper-mode +1)
  (popper-echo-mode +1)
  )


;;; tab-bar

(use-package tab-bar
  :ensure nil
  :config
  ;; Disable the "numeric argument". When used, I use the =C-u= prefix.
  ;; (dolist (prefix '("C-"))
  ;;  (global-unset-key (kbd (concat prefix "-")))
  ;;  (dotimes (i 10)
  ;;    (global-unset-key (kbd (concat prefix (number-to-string i))))))

  ;; Tabs for window layouts (tab-bar.el and prot-tab.el)
  ;; =C-<number>
  ;; (setq tab-bar-select-tab-modifiers '(control))

  (setq tab-bar-show t
        tab-bar-new-tab-group nil
        tab-bar-close-button-show nil
        ;; tab-bar-separator " ❘ "
        tab-bar-auto-width nil)
  (setq tab-bar-tab-hints t) ; for tab-bar-circle-number

  (setq tab-bar-tab-name-truncated-max 15
        tab-bar-tab-name-function #'tab-bar-tab-name-truncated) ;; #'tab-bar-tab-name-current ; default

  (setq tab-bar-format                    ; Emacs 28
        '(
          ;; tab-bar-separator
          ;; tab-bar-format-menu-bar
          tab-bar-format-tabs-groups
          ;; tab-bar-separator
          ;; tab-bar-format-add-tab ;; turn on tab-bar-new-button

          tab-bar-format-align-right
          tab-bar-format-global
          ))

  ;;   (setq tab-bar-auto-width-max '(160 20))

  (unless (display-graphic-p) ; terminal
    (setq auto-resize-tab-bars nil) ; important
    (setq tab-bar-separator nil) ; important
    )

  (tab-bar-history-mode +1)
  )

;;; extra.el ends here
