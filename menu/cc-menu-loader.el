;;; -*- mode: emacs-lisp; coding: utf-8; lexical-binding: t -*-

;;; cc-menu-loader

(require 'expand-region)
(require 'wgrep)
;; (require 'yasnippet)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq scroll-step 1)

;;; text-mode

;; (add-hook 'text-mode-hook 'context-menu-mode)
(add-hook 'org-mode-hook 'context-menu-mode)
(add-hook 'markdown-mode-hook 'context-menu-mode)

;;;; org-mode

;; (add-hook 'org-mode-hook (lambda ()
;; 			   (define-key org-mode-map (kbd "<f8>") 'datestamp)
;; 			   ;; (define-key org-mode-map (kbd "<f10>") 'avy-goto-word-1)
;; 			   (define-key org-mode-map (kbd "M-<f6>") 'org-toggle-inline-images)
;; 			   (define-key org-mode-map (kbd "C-c t") 'cc/org-time-stamp-inactive)
;; 			   (define-key org-mode-map (kbd "<home>") 'org-beginning-of-line)
;; 			   (define-key org-mode-map (kbd "<end>") 'org-end-of-line)
;; 			   (define-key org-mode-map (kbd "A-<left>") 'org-backward-sentence)
;; 			   (define-key org-mode-map (kbd "A-<right>") 'org-forward-sentence)
;; 			   (define-key org-mode-map (kbd "A-M-<left>") 'org-backward-paragraph)
;; 			   (define-key org-mode-map (kbd "A-M-<right>") 'org-forward-paragraph)
;; 			   (define-key org-mode-map (kbd "C-<up>") 'org-previous-visible-heading)
;; 			   (define-key org-mode-map (kbd "C-<down>") 'org-next-visible-heading)
;;                            (add-to-list (make-local-variable 'company-backends)
;;                                         'company-org-block)))


;;;; markdown-mode

;; (add-hook 'markdown-mode-hook 'variable-pitch-mode)
;; (add-hook 'markdown-mode-hook 'markdown-toggle-markup-hiding)
;; (add-hook 'markdown-mode-hook (lambda ()
;;                                 (turn-on-orgtbl)
;;                                 (define-key markdown-mode-map (kbd "C-<up>") 'markdown-backward-same-level)
;;                                 (define-key markdown-mode-map (kbd "C-<down>") 'markdown-forward-same-level)
;;                                 (define-key markdown-mode-map (kbd "M-<f6>") 'markdown-toggle-inline-images)
;;                                 (define-key markdown-mode-map [f13] 'markdown-preview)))


;;; cclisp utility functions

(require 'cclisp)

;; (add-hook 'shell-mode-hook 'context-menu-mode)
;; (add-hook 'dired-mode-hook 'context-menu-mode)

;;; prog-mode

;; (add-hook 'prog-mode-hook 'electric-pair-mode)
;; (add-hook 'prog-mode-hook 'context-menu-mode)

;;; repeat-mode

;; (defvar vifon/buffer-nextprev-repeat-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "<up>")    #'next-buffer)
;;     (define-key map (kbd "<right>") #'next-buffer)
;;     (define-key map (kbd "<down>")  #'previous-buffer)
;;     (define-key map (kbd "<left>")  #'previous-buffer)
;;     map))

;; ;;(put 'next-buffer     'repeat-map 'vifon/buffer-nextprev-repeat-map)
;; ;;(put 'previous-buffer 'repeat-map 'vifon/buffer-nextprev-repeat-map)

;; (defun repeatize (keymap)
;;   "Add `repeat-mode' support to a KEYMAP."
;;   (map-keymap
;;    (lambda (_key cmd)
;;      (when (symbolp cmd)
;;        (put cmd 'repeat-map keymap)))
;;    (symbol-value keymap)))

;; (defvar cc/org-header-navigation-repeat-map
;;   (let ((map (make-sparse-keymap)))
;;     (define-key map (kbd "p")    #'org-previous-visible-heading)
;;     (define-key map (kbd "n")  #'org-next-visible-heading)
;;     map))

;; (repeatize 'vifon/buffer-nextprev-repeat-map)
;; (repeatize 'cc/org-header-navigation-repeat-map)

;; (repeat-mode)



;;; ediff

(require 'ediff)

;; ;; these defvars are here to let cc-ediff-mode.el compile clean
(defvar ediff-buffer-A)
(defvar ediff-buffer-B)
(defvar ediff-buffer-C)
(defvar ediff-merge-job)
(defvar ediff-ancestor-buffer)

;; CC: I set my Ediff variables in `custom-set-variables'
;; Use your own preference.
;; '(ediff-keep-variants nil)
;; '(ediff-split-window-function 'split-window-horizontally)
;; '(ediff-window-setup-function 'ediff-setup-windows-plain)

(defvar cc/ediff-revision-session-p nil
  "If t then `cc/ediff-revision-actual' has been called.
This state variable is used to insert added behavior to the overridden
function `ediff-janitor'.")

(defun cc/ediff-revision-from-menu (e)
  "Invoke `ediff-revision' on E with variable `buffer-file-name'."
  (interactive "e")
  (cc/ediff-revision))

(defun cc/ediff-revision ()
  "Run Ediff on the current `buffer-file-name' provided that it is `vc-registered'.
This function handles the interactive concerns found in `ediff-revision'.
This function will also test if a diff should apply to the current buffer."
  (interactive)
  (when (and (bound-and-true-p buffer-file-name)
             (vc-registered (buffer-file-name)))
    (if (and (buffer-modified-p)
	     (y-or-n-p (format "Buffer %s is modified.  Save buffer? "
                               (buffer-name))))
        (save-buffer (current-buffer)))
    (message buffer-file-name)
    (cc/ediff-revision-actual))

  (cond ((not (bound-and-true-p buffer-file-name))
         (message (concat (buffer-name) " is not a file that can be diffed.")))
        ((not (vc-registered buffer-file-name))
         (message (concat buffer-file-name " is not under version control.")))))

(defun cc/ediff-revision-actual ()
  "Invoke Ediff logic to diff the modified repo file to its counterpart in the
current branch.
This function handles the actual diff behavior called by `ediff-revision'."
  (let ((rev1 "")
        (rev2 ""))
    (setq cc/ediff-revision-session-p t)
    (ediff-load-version-control)
    (funcall
     (intern (format "ediff-%S-internal" ediff-version-control-package))
     rev1 rev2 nil)))

(defun ediff-janitor (ask keep-variants)
  "Kill buffers A, B, and, possibly, C, if these buffers aren't modified.
In merge jobs, buffer C is not deleted here, but rather according to
`ediff-quit-merge-hook'.
ASK non-nil means ask the user whether to keep each unmodified buffer, unless
KEEP-VARIANTS is non-nil, in which case buffers are never killed.
A side effect of cleaning up may be that you should be careful when comparing
the same buffer in two separate Ediff sessions: quitting one of them might
delete this buffer in another session as well.

CC MODIFICATION: This method overrides the original Ediff function."
  (let ((ask (if (and (boundp 'cc/ediff-revision-session-p)
                      cc/ediff-revision-session-p)
                 nil
               ask)))
    (ediff-dispose-of-variant-according-to-user
     ediff-buffer-A 'A ask keep-variants)
    ;; !!!: CC Note: Test global state variable `cc/ediff-revision-session-p' to
    ;; determine if the modified repo file should be kept.
    ;; Guarding in place to hopefully avoid side-effects when `ediff-janitor' is
    ;; called from other Ediff functions. Informal testing has not revealed any
    ;; side-effects but YOLO.
    (if (and (boundp 'cc/ediff-revision-session-p)
             cc/ediff-revision-session-p)
        (ediff-dispose-of-variant-according-to-user
         ;; CC Note: keep-variants argument is hard-coded to t to keep
         ;; buffer holding modified repo file around.
         ediff-buffer-B 'B t t)
      (ediff-dispose-of-variant-according-to-user
       ediff-buffer-B 'B ask keep-variants))
    (if ediff-merge-job  ; don't del buf C if merging--del ancestor buf instead
        (ediff-dispose-of-variant-according-to-user
         ediff-ancestor-buffer 'Ancestor ask keep-variants)
      (ediff-dispose-of-variant-according-to-user
       ediff-buffer-C 'C ask keep-variants))
    ;; CC Note: Reset global state variable `cc/ediff-revision-session-p'.
    (if (and (boundp 'cc/ediff-revision-session-p)
             cc/ediff-revision-session-p)
        (setq cc/ediff-revision-session-p nil))))

(defun cc/stash-window-configuration-for-ediff ()
  "Store window configuration to register 🧊.
Use of emoji is to avoid potential use of keyboard character to reference
the register."
  (window-configuration-to-register ?🧊))

(defun cc/restore-window-configuration-for-ediff ()
  "Restore window configuration from register 🧊.
Use of emoji is to avoid potential use of keyboard character to reference
the register."
  (jump-to-register ?🧊))

(add-hook 'ediff-before-setup-hook #'cc/stash-window-configuration-for-ediff)
;; !!!: CC Note: Why this is not `ediff-quit-hook' I do not know. But this works
;; for cleaning up ancillary buffers on quitting an Ediff session.
(add-hook 'ediff-after-quit-hook-internal #'cc/restore-window-configuration-for-ediff)

;;; flyspell

(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

;;; cc-context-menu

(require 'cc-context-menu)

;;; cc-menu-reconfig

(require 'cc-menu-reconfig)

;; (when (string= (system-name) "bingsu.local")
;;   (server-start))

(provide 'cc-menu-loader)
