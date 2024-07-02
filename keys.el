;;; keys.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Junghan Kim
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; Maintainer: Junghan Kim <junghanacs@gmail.com>
;; Created: July 02, 2024
;; Modified: July 02, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/junghan0611/keys
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Global Map

(global-unset-key (kbd "<f1>"))  ; unset f1
(global-unset-key (kbd "<f2>"))
(global-unset-key (kbd "<f11>"))
(global-unset-key (kbd "M-c"))  ; unset capitalize-word

(global-set-key (kbd "<f1>") 'hydra-all/body)
(global-set-key (kbd "s-s") 'hydra-all/body)
(global-set-key (kbd "M-g 1") 'hydra-jump-to-directory/body)
(global-set-key (kbd "M-g 2") 'hydra-jump-to-files/body)
(global-set-key (kbd "M-g b") 'hydra-bm/body)

(global-set-key (kbd "M-c") 'major-mode-hydra)
(global-set-key (kbd "<f2>") 'major-mode-hydra)

;; 확실한 이지 가이드가 된다.

;; org-mode

(defvar-keymap ews-bibliography-map
  :doc "Bibliograpic functions keymap."
  "a" #'citar-denote-add-citekey

  "b" #'org-cite-insert
  "c" #'citar-open
  "d" #'citar-dwim

  ;; "e" #'citar-org-roam-cited ;; find cited heading
  "f" #'citar-denote-find-citation ;; grep [cite @xxx]

  "i" #'citar-insert-citation
  "n" #'citar-create-note
  "o" #'citar-open-note
  "l" #'citar-open-links

  ;; "r" #'citar-org-roam-open-current-refs
  "r" #'citar-denote-find-reference
  ;; "L" #'citar-denote-link-reference
  ;; "e" #'citar-denote-open-reference-entry
  ;; "k" #'citar-denote-remove-citekey
  )

(defvar-keymap ews-denote-map
  :doc "Denote keybindings."

  ;; "b" #'denote-show-backlinks-buffer
  "M-b" 'denote-find-backlink

  "d" #'denote-create-note
  "D" #'denote-org-dblock-insert-links

  ;; "o" #'consult-denote-open
  "o" #'denote-silo-extras-create-note
  "O" #'denote-silo-extras-open-or-create

  "i" #'denote-org-extras-dblock-insert-links
  "I" #'denote-org-extras-dblock-insert-backlinks
  "M-i" #'denote-org-extras-dblock-insert-files

  "l" #'denote-link-or-create
  "L" #'denote-link-after-creating-with-command

  "t" #'denote-type
  "k" #'denote-keywords-add
  "K" #'denote-keywords-remove
  )

(global-set-key (kbd "<f11>") ews-denote-map)
(keymap-set global-map "C-c n" ews-denote-map)

;; Org mode keymap modifications
(with-eval-after-load 'org
  (keymap-set org-mode-map "C-x n b" #'org-cite-insert)
  ;; (keymap-set org-mode-map "C-x n -" #'bh/insert-inactive-timestamp)
  (keymap-set org-mode-map "M-s ," #'denote-rename-file-using-front-matter)
  (keymap-set org-mode-map "C-x n 0" #'ews-org-insert-notes-drawer)
  (keymap-set org-mode-map "C-x n 9" #'ews-org-count-words)
  (keymap-set org-mode-map "C-x n 8" #'ews-org-insert-screenshot)
  )

;;; keys.el ends here
