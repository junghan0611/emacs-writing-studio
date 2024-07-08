;;; evil.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Junghan Kim
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; Maintainer: Junghan Kim <junghanacs@gmail.com>
;; Created: May 02, 2024
;; Modified: May 02, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/junghan0611/evil
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;;; macro

(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))

;;; evil

(use-package evil
  :demand t
  :hook ((after-init . evil-mode)
         (prog-mode . hs-minor-mode))
  :init
  (setq evil-want-keybinding nil
        evil-symbol-word-search t
        evil-ex-search-vim-style-regexp t
        evil-search-module 'evil-search
        ;; evil-magic 'very-magic
        hs-minor-mode-map nil)
  (setq evil-cross-lines t
        evil-kill-on-visual-paste nil
        evil-move-beyond-eol nil
	evil-want-C-g-bindings t
        evil-want-C-i-jump nil
        evil-want-fine-undo t
        evil-want-C-u-delete t
        evil-want-C-u-scroll t
        evil-v$-excludes-newline nil)
  (setq evil-want-C-w-delete t) ; default t
  (setq evil-want-C-u-scroll t) ; default t
  (setq evil-want-Y-yank-to-eol t)

  (setq
   evil-default-cursor '+evil-default-cursor-fn
   evil-normal-state-cursor 'box
   evil-emacs-state-cursor  '(box +evil-emacs-cursor-fn)
   evil-insert-state-cursor 'bar
   evil-visual-state-cursor 'hollow)

  (setq evil-visual-update-x-selection-p nil)

  :config

  (defun +evil-default-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-normal-color)))
  (defun +evil-emacs-cursor-fn ()
    (evil-set-cursor-color (get 'cursor 'evil-emacs-color)))

  ;; 'Important' Prevent the cursor from moving beyond the end of line.
  ;; Don't move the block cursor when toggling insert mode
  (setq evil-move-cursor-back nil) ; nil is better - default t
  (setq evil-move-beyond-eol nil) ; default nil

  ;; Don't put overwritten text in the kill ring
  (setq evil-kill-on-visual-paste nil) ; default t
  (setq evil-want-fine-undo t) ; doom 'nil

  ;; Don't create a kill entry on every visual movement.
  ;; More details: https://emacs.stackexchange.com/a/15054:
  (fset 'evil-visual-update-x-selection 'ignore)

  ;; Focus new window after splitting
  (setq evil-split-window-below t
        evil-vsplit-window-right t) ; default nil

  ;; Implicit /g flag on evil ex substitution, because I use the default behavior
  ;; less often.
  (setq evil-ex-substitute-global t) ; default nil

  )

;;; evil-nerd-commenter

(use-package evil-nerd-commenter
  :after evil
  :config
  ;; Turn on Evil Nerd Commenter
  (evilnc-default-hotkeys))

;;; evil-escape

(use-package evil-escape
  :after evil
  :init
  (setq evil-escape-key-sequence ",.") ;; "jk"
  (setq evil-escape-unordered-key-sequence nil)
  (setq evil-escape-delay 1.0) ;; 0.5, default 0.1
  :hook (after-init . evil-escape-mode)
  )

;;; evil-collection

(use-package evil-collection
  :hook (after-init . evil-collection-init)
  :init
  (add-hook 'org-agenda-mode-hook
            (lambda () (evil-collection-unimpaired-mode -1))))

;; (use-package evil-surround
;;   :straight t
;;   :hook ((text-mode prog-mode conf-mode) . evil-surround-mode)
;;   :config
;;   (add-hook 'emacs-lisp-mode-hook
;;             (lambda ()
;;               (push '(?` . ("`" . "'")) evil-surround-pairs-alist)))
;;   ;; `s' for surround instead of `subtitute'
;;   (general-def 'visual evil-surround-mode-map
;;     "s" 'evil-surround-region
;;     "S" 'evil-substitute))

(use-package evil-org
  :after evil
  :hook (org-mode . evil-org-mode)
  :init
  (setq evil-org-key-theme '(navigation textobjects additional calendar todo)) ; insert heading

  (with-eval-after-load 'org-agenda
    (autoload #'evil-org-agenda-set-keys "evil-org-agenda" nil t)
    (evil-org-agenda-set-keys))

  (with-eval-after-load 'org-capture
    (add-hook 'org-capture-mode-hook #'evil-insert-state)
    (add-hook 'org-capture-after-finalize-hook #'evil-normal-state)
    (evil-define-key 'normal org-capture-mode-map "ZZ" 'org-capture-finalize)
    (evil-define-key 'normal org-capture-mode-map "ZQ" 'org-capture-kill)
    (evil-define-key 'normal org-capture-mode-map "ZR" 'org-capture-refile)))

(with-eval-after-load 'vertico
  (keymap-set vertico-map "C-j" #'vertico-next)
  (keymap-set vertico-map "C-k" #'vertico-previous)
  (keymap-set vertico-map "M-h" #'vertico-directory-up))



;;; more motions

;; Rebind universal argument
;; (global-set-key (kbd "C-M-u") 'universal-argument)

;; Add binding for ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Replace Emacs Tabs key bindings with Workspace key bindings
(with-eval-after-load 'evil-maps
  ;; from DW
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-n") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-p") 'org-previous-visible-heading)
  (define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
  (define-key evil-motion-state-map "gc" 'evilnc-comment-operator)

  ;; replace "." search with consul-line in Evil normal state
  ;; use default "/" evil search
  (evil-global-set-key 'normal "." 'consult-line)

  (define-key evil-motion-state-map "L" nil)
  (define-key evil-motion-state-map "M" nil)

  (evil-global-set-key 'normal (kbd "DEL") 'evil-switch-to-windows-last-buffer) ; Backspace

  ;; evil macro
  (define-key evil-normal-state-map (kbd "q") 'nil) ; evil macro disable
  (define-key evil-normal-state-map (kbd "Q") 'evil-record-macro)

  ;; o :: ace-link-info 이거면 충분하다.
  (define-key evil-insert-state-map (kbd "C-]") 'forward-char)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  ;; (evil-define-key '(insert) prog-mode-map (kbd "C-k") 'kill-line)

  (define-key evil-normal-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-normal-state-map (kbd "C-e") 'evil-end-of-line-or-visual-line)
  (define-key evil-insert-state-map (kbd "C-a") 'evil-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e") 'evil-end-of-line-or-visual-line)
  ;; =C-w= 'insert 'evil-delete-backward-word
  ;; =C-w= 'visual 'evil-window-map
  )

;;; undo-fu

(use-package undo-fu
  :demand t
  :config
  ;; Increase undo history limits to reduce likelihood of data loss
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)
  ;; (evil-define-key 'normal 'global (kbd "C-r") #'undo-fu-only-redo)
  ;; (evil-define-key 'normal 'global "u" #'undo-fu-only-undo)

  ;; Undo-fu customization options
  ;; Undoing with a selection will use undo within that region.
  (setq undo-fu-allow-undo-in-region t)
  ;; Use the `undo-fu-disable-checkpoint' command instead of Ctrl-G `keyboard-quit' for non-linear behavior.
  (setq undo-fu-ignore-keyboard-quit t)

  ;; C-r 은 isearch-backward 가 기본
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo)
  ;; By default while in insert all changes are one big blob. Be more granular
  (setq evil-want-fine-undo t)

  (setq evil-undo-system 'undo-fu)
  (evil-set-undo-system 'undo-fu)
  )

;;; toggle for input-method

(with-eval-after-load 'evil

;;;###autoload
  (defun block-toggle-input-method ()
    (interactive)
    (message (format "Input method is disabled in <%s> state." evil-state)))

;;;###autoload
  (defun check-evil-cursor-state-between-window-switch ()
    (let ((type (pcase current-input-method
                  ('nil 'bar)
                  ("korean-hangul" 'hbar))))
      (setq-local evil-insert-state-cursor type)))

  (progn
    ;; keep evil insert cursor status per input-method
    ;; 2024-04-09 커서 상태 기반 한영 입력! 커서를 신뢰하라!
    ;; - 버퍼 전환 시 커서 상태 유지
    ;; - 커서를 보면 input-method 온오프를 알 수 있다.
    ;; - 한영 전환은 insert 모드에서만 가능
    (mapc (lambda (mode)
            (let ((keymap (intern (format "evil-%s-state-map" mode))))
              (define-key (symbol-value keymap) (kbd "<Hangul>") #'block-toggle-input-method)
              (define-key (symbol-value keymap) (kbd "S-SPC") #'block-toggle-input-method)))
          '(motion normal visual))

    (add-hook 'evil-insert-state-entry-hook 'check-evil-cursor-state-between-window-switch)

    (defadvice! change-cursor-after-toggle-input (fn &optional arg interactive)
      :around #'toggle-input-method
      :around #'set-input-method
      (funcall fn arg interactive)
      (let ((type (pcase current-input-method
                    ('nil 'bar)
                    ("korean-hangul" 'hbar))))
        (setq-local evil-insert-state-cursor type)))
    )
  )

;;; evil.el ends here
