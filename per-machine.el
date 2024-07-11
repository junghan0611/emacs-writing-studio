;;; per-machine.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Junghan Kim
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; Maintainer: Junghan Kim <junghanacs@gmail.com>
;; Created: July 03, 2024
;; Modified: July 03, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/junghan0611/per-machine
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;

;;; Code:

;;; User Profile

;; (defconst user-data-dir (file-name-as-directory (getenv "DATA_DIR")))

;; 나의 공개키는 다음 에서 확인 할수 있다.
;; https://meta.sr.ht/~junghanacs.keys, https://meta.sr.ht/~junghanacs.pgp

(setq user-full-name (if (getenv "USER_FULL_NAME")
                         (getenv "USER_FULL_NAME")
                       "John Doe"))

(setq user-mail-address (if (getenv "USER_MAIL_ADDRESS")
                            (getenv "USER_MAIL_ADDRESS")
                          "john.doe@example.com"))

;; Set my GPG key as the default key
(setq-default epa-file-encrypt-to (if (getenv "EPA_FILE_ENCRYPT_TO")
                                      (list (getenv "EPA_FILE_ENCRYPT_TO"))
                                    (list "ABCDEFGHIJKLMN")))

;;; Calendar

(setq user-calendar-latitude 37.26
      user-calendar-longitude 127.01
      user-calendar-location-name "Suwon, KR")

;;; Directory Path

(cond
 ((eq system-type 'windows-nt)
  (defconst user-org-directory "~/sync/winmacs/org/")
  )
 ;; ((eq system-type 'darwin) )
 ((eq system-type 'gnu/linux)
  (defconst user-org-directory (if (getenv "ORG_DIRECTORY")
                                   (getenv "ORG_DIRECTORY")
                                 "~/org/"))
  ))

;; for android linux windows 
;; (defconst user-org-directory "~/sync/winmacs/org/")

(defconst user-project-directory (if (getenv "PROJECT_DIRECTORY")
                                     (getenv "PROJECT_DIRECTORY")
                                   "~/git/"))

(defconst blog-admin-dir (concat user-project-directory "blog/"))

;;; Org-Mode

(setq org-directory user-org-directory)

(defun my/expand-org-file-name (filename)
  (expand-file-name filename org-directory))

(defun my/org-inbox-file () (my/expand-org-file-name "inbox.org"))
(defun my/org-tasks-file () (my/expand-org-file-name "tasks.org"))
(defun my/org-references-file () (my/expand-org-file-name "references.org"))
(defun my/org-diary-file () (my/expand-org-file-name "diary.org"))
(defun my/org-blog-file () (my/expand-org-file-name "blog.org"))
(defun my/org-work-file () (my/expand-org-file-name "work.org"))
(defun my/org-drill-file () (my/expand-org-file-name "drill.org"))

;; directory
(defun my/org-calendar-directory () (my/expand-org-file-name ".calendar/"))
(defun my/org-attachment-directory () (my/expand-org-file-name ".attach/"))

;;; emacs-type

(defcustom emacs-type 'doomemacs
  "Select Emacs Distribution Types"
  :group 'emacs
  :type  '(choice
           (const :tag "spacemacs" spacemacs)
           (const :tag "spacemacs" craftedemacs)
           (const :tag "doomemacs" doomemacs)
           (const :tag "vanillaemacs" vanillaemacs)))

(defun is-spacemacs() (eq emacs-type 'spacemacs))
(defun is-doomemacs() (eq emacs-type 'doomemacs))
(defun is-craftedemacs() (eq emacs-type 'craftedemacs))
(defun is-vanillaemacs() (eq emacs-type 'vanillaemacs))

;; (when (is-spacemacs) (message "I Love Spacemacs"))

;;; per-machine.el ends here
