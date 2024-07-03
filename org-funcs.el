;;; org-funcs.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Junghan Kim
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; Maintainer: Junghan Kim <junghanacs@gmail.com>
;; Created: July 02, 2024
;; Modified: July 02, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/junghan0611/core-funcs
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

;;; prot-org : org-id-headlines

;; https://writequit.org/articles/emacs-org-mode-generate-ids.html

;; Then we can define our own version of org-custom-id-get that calls org-id-new and creates a new property if one doesn't already exist
;; 그런 다음 org-id-new 을 호출하고 프로퍼티가 없는 경우 새 프로퍼티를 생성하는 org-custom-id-get 의 자체 버전을 정의할 수 있습니다.
;; (require 'org-id)
;; (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

;; Copied from this article (with minor tweaks from my side):
;; <https://writequit.org/articles/emacs-org-mode-generate-ids.html>.
(defun prot-org--id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.

If POM is nil, refer to the entry at point.  If the entry does
not have an CUSTOM_ID, the function returns nil.  However, when
CREATE is non nil, create a CUSTOM_ID if none is present already.
PREFIX will be passed through to `org-id-new'.  In any case, the
CUSTOM_ID of the entry is returned."
  (org-with-point-at pom
    (let ((id (org-entry-get nil "ID"))) ; "CUSTOM_ID"
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new (concat prefix "h")))
        (org-entry-put pom "ID" id) ; "CUSTOM_ID"
        (org-id-add-location id (format "%s" (buffer-file-name (buffer-base-buffer))))
        id)))))

;; And add a helper function that's interactive to add custom ids to all headlines in the buffer if they don't already have one.
;; 또한 버퍼의 모든 헤드라인에 사용자 지정 아이디가 없는 경우 대화형 도우미 기능을 추가하여 사용자 지정 아이디를 추가할 수 있습니다.

;;;###autoload
(defun prot-org-id-headlines ()
  "Add missing CUSTOM_ID to all headlines in current file."
  (interactive)
  (org-map-entries
   (lambda () (prot-org--id-get (point) t))))

;;;###autoload
(defun prot-org-id-headline ()
  "Add missing CUSTOM_ID to headline at point."
  (interactive)
  (prot-org--id-get (point) t))

;;;###autoload
(defun prot-org-ox-html ()
  "Streamline HTML export."
  (interactive)
  (org-html-export-as-html nil nil nil t nil))

;;;###autoload
(defun prot-org-ox-texinfo ()
  "Streamline Info export."
  (interactive)
  (org-texinfo-export-to-info))

;;; Denote

;;;; 13.3. Split an Org subtree into its own note
(defun my-denote-org-extract-subtree (&optional silo)
  "Create new Denote note using current Org subtree.
Make the new note use the Org file type, regardless of the value
of `denote-file-type'.

With an optional SILO argument as a prefix (\\[universal-argument]),
ask user to select a SILO from `my-denote-silo-directories'.

Use the subtree title as the note's title.  If available, use the
tags of the heading are used as note keywords.

Delete the original subtree."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Select a silo: " my-denote-silo-directories nil t))))
  (if-let ((text (org-get-entry))
           (heading (org-get-heading :no-tags :no-todo :no-priority :no-comment)))
      (let ((element (org-element-at-point))
            (tags (org-get-tags))
            (denote-user-enforced-denote-directory silo))
        (delete-region (org-entry-beginning-position)
                       (save-excursion (org-end-of-subtree t) (point)))
        (denote heading
                tags
                'org
                nil
                (or
                 ;; Check PROPERTIES drawer for :created: or :date:
                 (org-element-property :CREATED element)
                 (org-element-property :DATE element)
                 ;; Check the subtree for CLOSED
                 (org-element-property :raw-value
                                       (org-element-property :closed element))))
        (insert text))
    (user-error "No subtree to extract; aborting")))

;;;; 13.5. Use =dired-virtual-mode= for arbitrary file listings

(defcustom prot-eshell-output-buffer "*Exported Eshell output*"
  "Name of buffer with the last output of Eshell command.
Used by `prot-eshell-export'."
  :type 'string
  :group 'eshell)

(defcustom prot-eshell-output-delimiter "* * *"
  "Delimiter for successive `prot-eshell-export' outputs.
This is formatted internally to have newline characters before
and after it."
  :type 'string
  :group 'eshell)

(defun prot-eshell--command-prompt-output ()
  "Capture last command prompt and its output."
  (let ((beg (save-excursion
               (goto-char (eshell-beginning-of-input))
               (goto-char (point-at-bol)))))
    (when (derived-mode-p 'eshell-mode)
      (buffer-substring-no-properties beg (eshell-end-of-output)))))

;;;###autoload
(defun prot-eshell-export ()
  "Produce a buffer with output of the last Eshell command.
If `prot-eshell-output-buffer' does not exist, create it.  Else
append to it, while separating multiple outputs with
`prot-eshell-output-delimiter'."
  (interactive)
  (let ((eshell-output (prot-eshell--command-prompt-output)))
    (with-current-buffer (get-buffer-create prot-eshell-output-buffer)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (eq (point-min) (point-max))
          (insert (format "\n%s\n\n" prot-eshell-output-delimiter)))
        (goto-char (point-at-bol))
        (insert eshell-output)
        (switch-to-buffer-other-window (current-buffer))))))

;;;; efls/denote-signature-buffer

(defun efls/denote-signature-buffer ()
  (interactive)
  (switch-to-buffer "*denote-signatures*")
  (read-only-mode -1)
  (erase-buffer)
  (insert
   (shell-command-to-string
    "ls -l | awk /==/ | sed  's/--/=@/3' | sort -t '=' -Vk 3,3 | sed 's/=@/--/' "))
  (dired-virtual denote-directory)
  (denote-dired-mode +1)
  (auto-revert-mode -1))

;;;; my/denote-random-note

(defun my/denote-random-note-from-directory (directory)
  "Open a random denote from a specific denote directory."
  (let* ((denote-directory directory)
         (files (denote-directory-files)))
    (find-file (nth (random (length files)) files))))

(defun my/denote-random-note ()
  "Open a random denote."
  (interactive)
  (my/denote-random-note-from-directory denote-directory))

;;;; my/open-dired-denote-directory

(defun my/open-dired-denote-directory ()
  "Open dired at denote-directory"
  (interactive)
  (dired denote-directory))

;;;; my/denote-find-file

;; sync/man/dotsamples/vanilla/damiencassou-dotfiles-meow/init.el
(defun my/denote-find-file (filename)
  "Open FILENAME, a denote file.
Interactively ask which file to open with completion."
  (interactive (list (denote-file-prompt)))
  (find-file filename))

;;;; my/denote-grep

(defun my/denote-grep ()
  "Search within my notes."
  (interactive)
  (consult-ripgrep denote-directory))
;; (consult-ripgrep denote-directory "") ; 무슨 차이?

;;;; my/denote-attach

(defun my/denote-attach (file &optional description)
  "Save FILE in .attach directory and add a link in current buffer.
The link will contain DESCRIPTION as text."
  (interactive "*fSelect file to attach: \nMDescription: " org-mode)
  (let ((target-dir (expand-file-name ".attach" denote-directory)))
    (unless (file-directory-p target-dir)
      (make-directory target-dir))
    (let* ((target-basename (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
           (target-filename (make-temp-file
                             (expand-file-name (concat target-basename ".") target-dir)
                             nil
                             (concat "." (file-name-extension file)))))
      (copy-file file target-filename t)
      (org-insert-link nil (concat "file:" target-filename) description)
      (when (yes-or-no-p "Delete the initial file? ")
        (delete-file file t)))))

;;;; my/denote-info

;; ;; ###autoload
(defun my/denote-info ()
  "Count number of Denote text files,keywords and attachments."
  (interactive)
  (let* ((all-files (length (denote-directory-files)))
	 (denote-files (length (denote-directory-files nil nil t)))
	 (attachments (- all-files denote-files))
	 (keywords (length (denote-keywords))))
    (message "%s Denote files (%s Attachments), %s Distinct Keywords."
	     denote-files attachments keywords)))

;; (defun my/new-blog (title)
;;   (interactive "sTitle: ")
;;   (let ((filename (format "%s" title))
;;         (ext ".org"))
;;     (find-file (concat user-org-directory "/posts/" filename ext))
;;     (insert "#+TITLE: " title "\n")
;;     (tempel-insert 'blog)))

;; (defun my/new-meeting (meet)
;;   (interactive "sTitle: ")
;;   (let ((filename (format "%s-%s" (format-time-string "%Y%m%d") meet))
;;         (ext ".org"))
;;     (find-file (concat user-org-directory "/notes/" filename ext))
;;     (insert "#+TITLE: " meet "\n")
;;     (tempel-insert 'meeting)))

;;;; DONT autocalc-clocktable

;; https://200ok.ch/posts/2022-12-07_streamline_your_org_mode_workflow_with_automatic_clock_table_recalculation.html
;; Need add #+AUTOCALC_CLOCK_TABLES to org file.
;; (with-eval-after-load 'org
;;   (add-to-list 'org-options-keywords "AUTOCALC_CLOCK_TABLES:"))

;; (defun autocalc-clocktable ()
;;   "Auto update clock table."
;;   (when (derived-mode-p 'org-mode)
;;     (save-excursion
;;       (goto-char 0)
;;       (if (string-equal (car
;;                          (cdr
;;                           (car
;;                            (org-collect-keywords '("AUTOCALC_CLOCK_TABLES")))))
;;                         "t")
;;           (progn
;;             (goto-char (search-forward "clocktable"))
;;             (org-ctrl-c-ctrl-c))))))

;; (add-hook 'before-save-hook 'autocalc-clocktable)

;;;; my/org-find-time-file-property
;; https://github.com/zaeph/.emacs.d/blob/615ac37be6bd78c37e967fdb43d28897a4116583/lisp/zp-org.el#L194
(defun my/org-find-time-file-property (property &optional anywhere)
  "Return the position of the time file PROPERTY if it exists.
When ANYWHERE is non-nil, search beyond the preamble."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading)
                               t)
        (point)))))

;;;; my/org-set-time-file-property

;; (defun my/org-set-time-file-property (property &optional anywhere pos)
;;   "Set the time file PROPERTY in the preamble.
;; When ANYWHERE is non-nil, search beyond the preamble.
;; If the position of the file PROPERTY has already been computed,
;; it can be passed in POS."

;;   (when-let ((pos (or pos
;;                       (my/org-find-time-file-property property))))
;;     (save-excursion
;;       (goto-char pos)
;;       (if (looking-at-p " ")
;;           (forward-char)
;;         (insert " "))
;;       (delete-region (point) (line-end-position))
;;       (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
;;         (insert now)))))

;;;; my/org-set-date

;; https://github.com/zaeph/.emacs.d/blob/615ac37be6bd78c37e967fdb43d28897a4116583/lisp/zp-org.el#L212
;; (defun my/org-set-date ()
;;   "Update the LAST_MODIFIED file property in the preamble."
;;   (when (and (derived-mode-p 'org-mode)
;;              (buffer-modified-p))
;;     (my/org-set-time-file-property "DATE")))

;; (add-hook 'before-save-hook 'my/org-set-date)

;;;; my/reading-list

;; Get reading list from books directory for org-clock report.
;; The org-clock report scope can be a function.
;; (defun my/reading-list ()
;;   "Get reading list."
;;   (let (reading-list)
;;     (append reading-list
;;             (file-expand-wildcards (expand-file-name "bib/*.org" user-org-directory)))))

;; (defun prompt-for-note-title ()
;;   "Prompt the user to enter the title for the note using minibuffer."
;;   (read-string "Enter note title: "))

;;;; org-create-id-by-denote-identifier at-once

(defun my/denote-identifier-retrieve ()
  (let* ((file (or (buffer-file-name) (dired-get-filename))))
    (when file
      (denote-retrieve-filename-identifier file))))

(defun my/org-create-id-by-denote-identifier()
  (interactive)
  (org-entry-put nil "ID" (my/denote-identifier-retrieve)))

(defun my/org-create-id-by-denote-identifier-at-once (directory extension)
  (interactive (list (read-directory-name "Directory: ")
                     (read-string "File extension: ")))
  (dolist (file (directory-files-recursively directory extension))
    (find-file file)
    (my/org-create-id-by-denote-identifier)
    (save-buffer)
    (kill-buffer nil)))

;;;; denote-sort

;;;###autoload
(defun my/goto-denote-dired (&optional _)
  (interactive)
  (let ((buf (get-buffer "denote-sort-signature")))
    (tab-bar-switch-to-tab "note")
    (if buf
        (progn (switch-to-buffer buf)
               (delete-other-windows))
      (denote-sort-dired nil 'signature nil)
      (rename-buffer "denote-sort-signature")
      (spacemacs/toggle-current-window-dedication) ; spacemacs Compatibility
      )))

(defun my/denote-signature-retrieve ()
  (let* ((file (or (buffer-file-name) (dired-get-filename))))
    (when file
      (denote-retrieve-filename-signature file))))

(defun my/denote-sort-regexp (regexp)
  (interactive (list
	        (read-regexp
	         (concat "Files matching PATTERN" (format " (default: %s)" (my/denote-signature-retrieve)) ": ")
	         (my/denote-signature-retrieve)
	         nil)))
  (denote-sort-dired (concat "==" regexp) 'signature nil))

(defun my/denote-sort-with-identifer ()
  (interactive)
  (denote-sort-dired (denote-files-matching-regexp-prompt) 'identifier nil))

(defun my/denote-sort-with-keywords ()
  (interactive)
  (denote-sort-dired (regexp-opt (denote-keywords-prompt)) 'keywords nil))

(defun my/denote-sort-with-days ()
  (interactive)
  (let ((regexp (call-interactively 'my/denote-week-ago)))
    (denote-sort-dired regexp 'signature nil)))

(defun my/denote-sort-parent-with-children ()
  (interactive)
  (let* ((index (my/denote-signature-retrieve))
	 (length (length index))
	 (regexp (substring index 0 (- length 1))))
    (denote-sort-dired (concat "==" regexp) 'signature nil)))

(defun my/denote-sort-children-regexp ()
  (let* ((index (my/denote-signature-retrieve)))
    (format "==%s" index)))

(defun my/denote-sort-children ()
  (interactive)
  (let ((regexp (my/denote-sort-children-regexp)))
    (denote-sort-dired regexp 'signature nil)))

(defun my/denote-sort-siblings-regexp ()
  (let* ((index (my/denote-signature-retrieve))
	 (last-char (substring index (1- (length index)))))
    (if (string-match "[0-9]" last-char)
	(format "==\\(%s\\|%s[a-z]\\)-" index index)
      (format "==\\(%s\\|%s[0-9]+\\)-" index index))))

(defun my/denote-sort-siblings ()
  (interactive)
  (let ((regexp (my/denote-sort-siblings-regexp)))
    (denote-sort-dired regexp 'signature nil)))

;; fliter denote create by days ago
;;;###autoload
(defun my/denote-week-ago ()
  (interactive)
  (let* ((current-time (current-time))
	 (current-date (format-time-string "%Y-%m-%d" current-time))
	 (ago-date-time (time-subtract current-time (days-to-time 14))) ;; 7
	 (ago-date (format-time-string "%Y-%m-%d" ago-date-time))
	 (cur-year (substring current-date 0 4))
	 (cur-month (substring current-date 5 7))
	 (cur-day (substring current-date 8 10))
	 (ago-year (substring ago-date 0 4))
	 (ago-month (substring ago-date 5 7))
	 (ago-day (substring ago-date 8 10))
	 (cur-day-d1 (/ (string-to-number cur-day) 10))
	 (cur-day-d2 (% (string-to-number cur-day) 10))
	 (ago-day-d1 (/ (string-to-number ago-day) 10))
	 (ago-day-d2 (% (string-to-number ago-day) 10)))
    (if (string= cur-year ago-year)
	(if (string= cur-month ago-month)
	    (if (= cur-day-d1 ago-day-d1)
		(format "\\(%s%s%s[%s-%s]\\)"
			cur-year cur-month cur-day-d1
			ago-day-d2 cur-day-d2)
	      (format "%s%s\\(%s[%s-9]\\|%s[0-%s]\\)"
		      cur-year cur-month ago-day-d1
		      ago-day-d2 cur-day-d1 cur-day-d2))
	  (cond ((< cur-day-d1 ago-day-d1)
		 (format "\\(%s\\)\\(%s%s[%s-9]\\|3[0-1]\\|%s%s[0-%s]\\)"
			 cur-year ago-month ago-day-d1 ago-day-d2
			 cur-month cur-day-d1 cur-day-d2))
		(t
		 (format "\\(%s\\)\\(%s%s[%s-9]\\|%s%s[0-%s]\\)"
			 cur-year ago-month ago-day-d1 ago-day-d2
			 cur-month cur-day-d1 cur-day-d2))))
      (if (= ago-day-d1 3)
	  (format "\\(%s123[%s-1]\\|%s010[0-%s]\\)"
		  ago-year ago-day-d2
		  cur-year cur-day-d2)
	(format "\\(%s12%s[%s-9]\\|%s123[0-1]\\|%s01%s[0-%s]\\)"
		ago-year ago-day-d1 ago-day-d2
		ago-year cur-year cur-day-d1 cur-day-d2)))))

(defun my/denote-sort-sigature-lv1 ()
  (interactive)
  (let ((regexp (call-interactively 'my/denote-sort-lv-1)))
    (denote-sort-dired regexp 'signature nil)))

(defun my/denote-sort-sigature-lv2 ()
  (interactive)
  (let ((regexp (call-interactively 'my/denote-sort-lv-2)))
    (denote-sort-dired regexp 'signature nil)))

(defun my/denote-sort-lv-2 (lv)
  (interactive "nInput the Level of Signature: ")
  (format "\\(==%s[a-z]-\\)" lv))

(defun my/denote-sort-lv-1 ()
  (interactive)
  (format "\\(==[0-9]-\\)"))

;;;; mho denote functions

;;   (defun +denote-open-note-directory ()
;;     (interactive)
;;     (dired denote-directory))

;; (defun +denote-find-note-file ()
;;   (interactive)
;;   (let ((default-directory denote-directory))
;;     (call-interactively #'+default/find-file-under-here)))

(defun mho-insert-denote-identifier ()
  "Get the file's creation date and time and u se it to create a denote identifier."
  (interactive)
  (insert (format-time-string "#+identifier: %Y%m%dT%H%M%S" (nth 5 (file-attributes buffer-file-name)))))

(defun mho-insert-org-date ()
  "Get the file's creation date and time and use it to insert the date using an org format."
  (interactive)
  (insert (format-time-string "#+date: [%Y-%m-%d %a %H:%M]" (nth 5 (file-attributes buffer-file-name)))))

;; (defun denote-subdirectory-new ()
;;   "Creates sub directory in the `denote-directory' for better organization"
;;   (interactive)
;;   (if-let (sd (read-string "Subdir name: " nil))
;;       (let ((subdir (file-name-concat denote-directory sd)))
;;         (if (f-dir? subdir)
;;             (message (concat "directory " subdir " already exists!"))
;;           (make-directory subdir))
;;         (denote-subdirectory subdir (denote--title-prompt) (denote--keywords-prompt)))))

;; (defun denote-browse ()
;;   "Browse files from `denote-directory'"
;;   (interactive)
;;   (unless (bound-and-true-p denote-directory)
;;     (message "denote-directoy not defined"))
;;   (doom-project-browse (concat denote-directory "/")))

;; (defun denote-subdirectory-with-date ()
;;   "Like `denote-subdirectory' but ask for date of the note."
;;   (interactive)
;;   (let ((denote-prompts '(title keywords date subdirectory)))
;;     (call-interactively #'denote)))

