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

(defconst blog-admin-dir "~/git/blog/")

(defconst user-org-directory (if (getenv "ORG_DIRECTORY")
                                 (getenv "ORG_DIRECTORY")
                               "~/org/"))

(defconst user-project-directory (if (getenv "PROJECT_DIRECTORY")
                                     (getenv "PROJECT_DIRECTORY")
                                   "~/git/"))

;;; Org-Mode

(if (boundp 'user-org-directory)
    (setq org-directory user-org-directory)
  (setq org-directory "~/org/"))

(defun my/expand-org-file-name (filename)
  (expand-file-name filename org-directory))

(defun my/org-index-file () (my/expand-org-file-name "20240429T165725--index.org"))

;; agenda
(defun my/org-inbox-file () (my/expand-org-file-name "agenda/20230202T020200--inbox.org"))
(defun my/org-contacts-file () (my/expand-org-file-name "agenda/20230303T030300--contacts.org"))
(defun my/org-tasks-file () (my/expand-org-file-name "agenda/20230101T010100--tasks.org"))
(defun my/org-links-file () (my/expand-org-file-name "agenda/20230219T035500--links.org"))
(defun my/org-diary-file () (my/expand-org-file-name "agenda/20220101T010100--diary.org"))
(defun my/org-drill-file () (my/expand-org-file-name "agenda/20240124T164402--drill.org"))
(defun my/org-mobile-file () (my/expand-org-file-name "agenda/20240312T111900--mobile.org"))
(defun my/org-quote-file () (my/expand-org-file-name "agenda/20240312T031200--quote.org"))
(defun my/org-life-file () (my/expand-org-file-name "agenda/20240327T112315--life.org"))

;; meta
(defun my/org-kdc-file () (my/expand-org-file-name "meta/20240312T142358--kdc.org"))
(defun my/org-tags-file () (my/expand-org-file-name "meta/20231005T133900--tags.org"))
(defun my/org-glossary-file () (my/expand-org-file-name "dict/global.org"))

;; posts
(defun my/org-blog-file () (my/expand-org-file-name "posts/20240104T061355--blog.org"))
(defun my/org-reading-file () (my/expand-org-file-name "posts/20240329T154123--reading.org"))

;; notes
(defun my/org-now-file () (my/expand-org-file-name "notes/20240618T125104--now.org"))
(defun my/org-remark-file () (my/expand-org-file-name "notes/20231111T094444--remark.org"))
(defun my/org-remember-file () (my/expand-org-file-name "notes/20231020T210500--remember.org"))

;; directory
(defun my/org-calendar-directory () (my/expand-org-file-name ".calendar/"))
(defun my/org-attachment-directory () (my/expand-org-file-name ".attach/"))

(defvar config-bibfiles (list "~/sync/org/bib/zotero-biblatex.bib"))

;;; per-machine.el ends here
