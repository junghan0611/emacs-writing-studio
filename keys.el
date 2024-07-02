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

;;; outli

(progn
  ;; Tab for Heading : outline-mode and org-mode
  ;; search to narrow with heading and tag base on built-in outline-mode
  ;; evil normal keybinding is perfer
  (evil-define-key '(normal visual) outli-mode-map (kbd "S-<tab>") `(menu-item "" ,(lambda () (interactive) (outline-cycle -1)) :filter outli--on-heading))
  (evil-define-key '(normal visual) outli-mode-map (kbd "S-TAB") `(menu-item "" ,(lambda () (interactive) (outline-cycle -1)) :filter outli--on-heading))
  (evil-define-key '(normal visual) outli-mode-map (kbd "<backtab>") `(menu-item "" ,(lambda () (interactive) (outline-cycle -1)) :filter outli--on-heading))
  (evil-define-key '(normal visual) outli-mode-map (kbd "S-<iso-lefttab>") `(menu-item "" ,(lambda () (interactive) (outline-cycle -1)) :filter outli--on-heading))

  ;; 'TAB' for terminal emacs
  (evil-define-key '(normal visual) outli-mode-map (kbd "<tab>") `(menu-item "" outline-cycle :filter outli--on-heading))
  (evil-define-key '(normal visual) outli-mode-map (kbd "TAB") `(menu-item "" outline-cycle :filter outli--on-heading))

  (evil-define-key '(normal visual) prog-mode-map (kbd "<tab>") 'indent-for-tab-command)
  (evil-define-key '(normal visual) prog-mode-map (kbd "TAB") 'indent-for-tab-command)

  (evil-define-key '(normal) outli-mode-map (kbd "C-c 1") (lambda () (interactive) (outline--show-headings-up-to-level 1)))
  (evil-define-key '(normal) outli-mode-map (kbd "C-c 2") (lambda () (interactive) (outline--show-headings-up-to-level 2)))
  (evil-define-key '(normal) outli-mode-map (kbd "C-c 3") (lambda () (interactive) (outline--show-headings-up-to-level 3)))
  (evil-define-key '(normal) outli-mode-map (kbd "C-c 4") (lambda () (interactive) (outline--show-headings-up-to-level 4)))
  (evil-define-key '(normal) outli-mode-map (kbd "C-c 5") (lambda () (interactive) (outline--show-headings-up-to-level 5)))
  (evil-define-key '(normal) outli-mode-map (kbd "C-M-<tab>") 'outline-cycle-buffer)

  ;; (define-key outli-mode-map (kbd "C-M-<iso-lefttab>")
  ;;             (lambda () (interactive) (outline-cycle-buffer)))

  (evil-define-key '(normal visual) outli-mode-map (kbd "C-n") 'outline-next-heading)
  (evil-define-key '(normal visual) outli-mode-map (kbd "C-p") 'outline-previous-heading)

  (evil-define-key '(insert) outli-mode-map (kbd "C-n") 'next-line)
  (evil-define-key '(insert) outli-mode-map (kbd "C-p") 'previous-line)

  (evil-define-key '(normal visual) outline-mode-map (kbd "C-S-p") 'outline-up-heading)
  (evil-define-key '(normal visual) outline-mode-map "zu" 'outline-up-heading)

  (define-key prog-mode-map (kbd "C-c H") 'outline-insert-heading)
  (define-key prog-mode-map (kbd "C-c o") 'consult-outline)
  )

;;; window

;; 편집 창 포커스 이동을 간단하게
(progn
  (global-set-key (kbd "M-s-l") 'evil-window-right)
  (global-set-key (kbd "M-s-h") 'evil-window-left)
  (global-set-key (kbd "M-s-]") 'evil-window-right)
  (global-set-key (kbd "M-s-[") 'evil-window-left)
  (global-set-key (kbd "M-s-k") 'evil-window-up)
  (global-set-key (kbd "M-s-j") 'evil-window-down))

;; If you use a window manager be careful of possible key binding clashes
;; (global-unset-key (kbd "M-<tab>"))
(global-set-key (kbd "M-<tab>") 'other-window) ; very useful
(global-set-key (kbd "M-<iso-lefttab>") (lambda() (interactive) (other-window -1))) ; == M-S-<tab>
(global-set-key (kbd "M-<backtab>") (lambda() (interactive) (other-window -1))) ; for terminal

(global-set-key (kbd "C-c <left>") 'winner-undo) ; built-in winner
(global-set-key (kbd "C-c <right>") 'winner-redo)

;;; expand-region

(when (locate-library "expand-region")
  (with-eval-after-load 'expand-region
    (evil-define-key '(normal visual) prog-mode-map (kbd "M-<up>") 'er/expand-region)
    (evil-define-key '(normal visual) prog-mode-map (kbd "M-<down>") 'er/contract-region)
    (evil-define-key '(normal visual) org-mode-map (kbd "M-<up>") 'er/expand-region)
    (evil-define-key '(normal visual) org-mode-map (kbd "M-<down>") 'er/contract-region)
    (evil-define-key '(normal visual) markdown-mode-map (kbd "M-<up>") 'er/expand-region)
    (evil-define-key '(normal visual) markdown-mode-map (kbd "M-<down>") 'er/contract-region)
    )
  )

;;; tab-bar menu-bar

;; gb / gB
;; Ctrl + Number
(with-eval-after-load 'tab-bar
  (define-key evil-motion-state-map "gb" 'tab-next)
  (define-key evil-motion-state-map "gB" 'tab-previous)
  (define-key evil-normal-state-map "gb" 'tab-next)
  (define-key evil-normal-state-map "gB" 'tab-previous)

  (define-key evil-motion-state-map "gh" 'menu-bar-open)
  (define-key evil-normal-state-map "gh" 'menu-bar-open))

(global-set-key (kbd "s-\\") 'tab-bar-switch-to-tab)
(global-set-key (kbd "s-[") 'tab-bar-switch-to-prev-tab) ; +tabs:previous-or-goto
(global-set-key (kbd "s-]") 'tab-bar-switch-to-next-tab) ; +tabs:next-or-goto

;;; evil-org

(with-eval-after-load 'evil-org
  ;; (evil-define-key 'insert 'evil-org-mode-map (kbd "C-d") 'delete-forward-char)
  (evil-define-key 'normal 'evil-org-mode-map "x" 'delete-forward-char)
  (evil-define-key 'insert 'evil-org-mode-map (kbd "C-k") 'org-kill-line)
  (evil-define-key 'insert 'org-mode-map (kbd "C-k") 'org-kill-line)
  (evil-define-key 'normal 'evil-org-mode-map "X" 'delete-backward-char)
  )

;;; embark - doom vs. spacemacs style

;; C-; embark-ack ; doom default
;; C-c C-; embark-export
;; C-c C-e ; +vertico/embark-export-write

(global-set-key (kbd "M-o") 'embark-act) ;; spacemacs bindings
(global-set-key (kbd "C-;") 'embark-dwim) ;; good alternative: M-.

(global-set-key (kbd "C-h B") 'embark-bindings) ;; alternative for `describe-bindings'


;;; keys.el ends here
