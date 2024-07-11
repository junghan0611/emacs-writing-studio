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

;;; eldoc

(global-set-key (kbd "C-M-'") 'eldoc-toggle)

;;; dired

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    (kbd "C-c C-e") 'wdired-change-to-wdired-mode
    (kbd "C-c l") 'org-store-link
    (kbd "C-x /") 'dired-narrow-regexp
    (kbd ".") 'consult-line
    (kbd "K") 'dired-kill-subdir
    (kbd "f") 'evil-avy-goto-line-below ;; 2024-01-25 useful
    (kbd "h") 'dired-up-directory
    (kbd "l") 'dired-find-alternate-file
    (kbd "S-SPC") 'dired-toggle-marks
    ;; <normal-state> RET            dired-find-file
    ;; <normal-state> S-<return>     dired-find-file-other-window
    )
  )

;;; consult

(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ;; ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s b" . consult-buffer)
         ("M-s f" . consult-find)
         ("M-s F" . consult-fd)
         ("M-s L" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s m" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line))                 ;; needed by consult-line to detect isearch
  )

;;; vertico

(with-eval-after-load 'vertico
  ;; 2023-12-02
  (define-key vertico-map (kbd"C-<return>") 'vertico-quick-exit)

  (define-key vertico-map (kbd "RET")   'vertico-directory-enter)
  (define-key vertico-map (kbd "DEL")   'vertico-directory-delete-char)
  (define-key vertico-map (kbd "M-DEL") 'vertico-directory-delete-word)

  (define-key vertico-map (kbd "C-S-j") #'vertico-scroll-up) ; default
  (define-key vertico-map (kbd "C-S-k") #'vertico-scroll-down)

  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous)
  (define-key vertico-map (kbd "C-M-j") #'vertico-next-group)
  (define-key vertico-map (kbd "C-M-k") #'vertico-previous-group)

  ;; 2023-05-23 org-roam-node-find
  (define-key vertico-map (kbd "C-n") #'spacemacs/next-candidate-preview)
  (define-key vertico-map (kbd "C-p") #'spacemacs/previous-candidate-preview)

  (unless (display-graphic-p) ; terminal
    (define-key vertico-map (kbd "M-<return>") #'vertico-exit-input))

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


;;; denote

(when (locate-library "denote")
  (defvar-keymap ews-denote-map
    :doc "Denote keybindings."

    "0" #'my/denote-info
    "a" #'my/denote-attach

    "b" #'denote-show-backlinks-buffer
    "M-b" 'denote-find-backlink

    "d" #'denote-create-note
    "D" #'denote-org-dblock-insert-links

    "f" #'my/denote-find-file
    "M-f" #'denote-find-link

    "e" #'prot-eshell-export

    ;; "o" #'consult-denote-open
    "o" #'denote-silo-extras-create-note
    "O" #'denote-silo-extras-open-or-create

    "i" #'denote-org-extras-dblock-insert-links
    "I" #'denote-org-extras-dblock-insert-backlinks
    "M-i" #'denote-org-extras-dblock-insert-files

    "l" #'denote-link-or-create
    "L" #'denote-link-after-creating-with-command

    "n" #'my/goto-denote-dired

    "m" #'denote-add-missing-links

    "g" #'my/denote-grep

    "t" #'denote-type

    "r" #'denote-region ; "contents" mnemonic
    ;; "r" #'denote-rename-file
    ;; "R" #'denote-rename-file-using-front-matter
    "," #'denote-rename-file-using-front-matter
    "?" #'my/denote-random-note

    ;; "s" #'denote-subdirectory
    "s" #'denote-sort-dired
    "S" #'my/denote-sort-with-days

    "k" #'denote-keywords-add
    "K" #'denote-keywords-remove

    ;; "z" #'denote-signature ; "zettelkasten" mnemonic
    ;; "Z" #'efls/denote-signature-buffer
    )

  (global-set-key (kbd "C-c n") ews-denote-map)
  )

;;; org-mode-map

;; Org mode keymap modifications
(with-eval-after-load 'org
  (keymap-set org-mode-map "C-x n b" #'org-cite-insert)
  (keymap-set org-mode-map "C-x n -" #'bh/insert-inactive-timestamp)
  (keymap-set org-mode-map "M-s ," #'denote-rename-file-using-front-matter)
  ;; (keymap-set org-mode-map "C-c n o" ews-org-noter-map)
  ;; (keymap-set org-mode-map "C-c n u" ews-org-transclusion-map)
  (keymap-set org-mode-map "C-x n 0" #'ews-org-insert-notes-drawer)
  (keymap-set org-mode-map "C-x n 9" #'ews-org-count-words)
  (keymap-set org-mode-map "C-x n 8" #'ews-org-insert-screenshot)
  )

;;; ZK zk-index and zk-desktop

(when (locate-library "zk-index")
  (with-eval-after-load 'zk-index
    (evil-define-key '(normal visual) zk-index-mode-map (kbd "n") #'zk-index-next-line)
    (evil-define-key '(normal visual) zk-index-mode-map (kbd "p") #'zk-index-previous-line)
    (evil-define-key '(normal visual) zk-index-mode-map (kbd "v") #'zk-index-view-note)
    (evil-define-key '(normal visual) zk-index-mode-map (kbd "o") #'other-window)
    (evil-define-key '(normal visual) zk-index-mode-map (kbd "f") #'zk-index-focus)
    (evil-define-key '(normal visual) zk-index-mode-map (kbd "s") #'zk-index-search)
    (evil-define-key '(normal visual) zk-index-mode-map (kbd "g") #'zk-index-query-refresh)
    (evil-define-key '(normal visual) zk-index-mode-map (kbd "c") #'zk-index-current-notes)
    (evil-define-key '(normal visual) zk-index-mode-map (kbd "i") #'zk-index-refresh)
    (evil-define-key '(normal visual) zk-index-mode-map (kbd "S") #'zk-index-sort-size)
    (evil-define-key '(normal visual) zk-index-mode-map (kbd "M") #'zk-index-sort-modified)
    (evil-define-key '(normal visual) zk-index-mode-map (kbd "C") #'zk-index-sort-created)
    (evil-define-key '(normal visual) zk-index-mode-map (kbd "RET") #'zk-index-open-note)
    (evil-define-key '(normal visual) zk-index-mode-map (kbd "q") #'delete-window)
    )
  )


;;; keys.el ends here
