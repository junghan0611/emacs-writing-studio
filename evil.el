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
  :config
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

  ;; (setq evil-insert-state-cursor '(box "#F86155")) ;; better look
  ;; (setq evil-normal-state-cursor '(box "DarkGoldenrod2"))

  (setq evil-normal-state-cursor  '("DarkGoldenrod2" box)
        evil-insert-state-cursor  '("chartreuse3" (bar . 2))
        evil-emacs-state-cursor   '("SkyBlue2" box)
        evil-replace-state-cursor '("chocolate" (hbar . 2))
        evil-visual-state-cursor  '("gray" (hbar . 2))
        evil-motion-state-cursor  '("plum3" box))

  (progn
    ;; Thanks to `editorconfig-emacs' for many of these
    (defvar evil-indent-variable-alist
      ;; Note that derived modes must come before their sources
      '(((awk-mode c-mode c++-mode java-mode
	  idl-mode java-mode objc-mode pike-mode) . c-basic-offset)
        (groovy-mode . groovy-indent-offset)
        (python-mode . python-indent-offset)
        (cmake-mode . cmake-tab-width)
        (coffee-mode . coffee-tab-width)
        (cperl-mode . cperl-indent-level)
        (css-mode . css-indent-offset)
        (elixir-mode . elixir-smie-indent-basic)
        ((emacs-lisp-mode lisp-mode) . lisp-indent-offset)
        (enh-ruby-mode . enh-ruby-indent-level)
        (erlang-mode . erlang-indent-level)
        (js2-mode . js2-basic-offset)
        (js3-mode . js3-indent-level)
        ((js-mode json-mode) . js-indent-level)
        (latex-mode . (LaTeX-indent-level tex-indent-basic))
        (livescript-mode . livescript-tab-width)
        (mustache-mode . mustache-basic-offset)
        (nxml-mode . nxml-child-indent)
        (perl-mode . perl-indent-level)
        (puppet-mode . puppet-indent-level)
        (ruby-mode . ruby-indent-level)
        (rust-mode . rust-indent-offset)
        (scala-mode . scala-indent:step)
        (sgml-mode . sgml-basic-offset)
        (sh-mode . sh-basic-offset)
        (typescript-mode . typescript-indent-level)
        (web-mode . web-mode-markup-indent-offset)
        (yaml-mode . yaml-indent-offset))
      "An alist where each key is either a symbol corresponding
  to a major mode, a list of such symbols, or the symbol t,
  acting as default. The values are either integers, symbols
  or lists of these.")

    (defun set-evil-shift-width ()
      "Set the value of `evil-shift-width' based on the indentation settings of the
  current major mode."
      (let ((shift-width
             (catch 'break
               (dolist (test evil-indent-variable-alist)
                 (let ((mode (car test))
                       (val (cdr test)))
                   (when (or (and (symbolp mode) (derived-mode-p mode))
                             (and (listp mode) (apply 'derived-mode-p mode))
                             (eq 't mode))
                     (when (not (listp val))
                       (setq val (list val)))
                     (dolist (v val)
                       (cond
                        ((integerp v) (throw 'break v))
                        ((and (symbolp v) (boundp v))
                         (throw 'break (symbol-value v))))))))
               (throw 'break (default-value 'evil-shift-width)))))
        (when (and (integerp shift-width)
                   (< 0 shift-width))
          (setq-local evil-shift-width shift-width))))

    ;; after major mode has changed, reset evil-shift-width
    (add-hook 'after-change-major-mode-hook #'set-evil-shift-width 'append))

  (progn
    (evil-define-text-object evil-pasted (count &rest args)
      (list (save-excursion (evil-goto-mark ?\[) (point))
            (save-excursion (evil-goto-mark ?\]) (1+ (point)))))
    (define-key evil-inner-text-objects-map "P" 'evil-pasted)

    ;; define text-object for entire buffer
    (evil-define-text-object evil-inner-buffer (count &optional beg end type)
      (list (point-min) (point-max)))
    (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer))

  ;; allow eldoc to trigger directly after changing modes
  (eldoc-add-command #'evil-normal-state
                     #'evil-insert
                     #'evil-change
                     #'evil-delete
                     #'evil-replace)

  (add-hook 'evil-normal-state-exit-hook #'evil-ex-nohighlight)
  (evil-define-key 'normal org-capture-mode-map "zf" 'reposition-window)
  ;; (general-def 'insert [remap evil-complete-previous] 'hippie-expand)
  )

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
  (setq evil-org-key-theme '(navigation insert textobjects additional todo heading))

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

(use-package evil-nerd-commenter
  :after evil
  :config
  ;; Turn on Evil Nerd Commenter
  (evilnc-default-hotkeys))

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

  ;; o :: ace-link-info 이거면 충분하다.
  (define-key evil-insert-state-map (kbd "C-]") 'forward-char)

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

;;; evil.el ends here
