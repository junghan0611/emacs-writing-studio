;;; core-funcs.elG -*- lexical-binding: t; -*-
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

(require 'pcre2el)

;;;; File

(defun rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.

If called without a prefix argument, the prompt is
initialized with the current directory instead of filename."
  (interactive "P")
  (let* ((old-short-name (buffer-name))
         (old-filename (buffer-file-name)))
    (if (and old-filename (file-exists-p old-filename))
        ;; the buffer is visiting a file
        (let* ((old-dir (file-name-directory old-filename))
               (new-name (read-file-name "New name: " (if arg old-dir old-filename)))
               (new-dir (file-name-directory new-name))
               (new-short-name (file-name-nondirectory new-name))
               (file-moved-p (not (string-equal new-dir old-dir)))
               (file-renamed-p (not (string-equal new-short-name old-short-name))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                ((string-equal new-name old-filename)
                 (error "Rename failed! Same new and old name")
                 (rename-current-buffer-file))
                (t
                 (let ((old-directory (file-name-directory new-name)))
                   (when (and (not (file-exists-p old-directory))
                              (yes-or-no-p
                               (format "Create directory '%s'?" old-directory)))
                     (make-directory old-directory t)))
                 (rename-file old-filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept old-filename))
                 (message (cond ((and file-moved-p file-renamed-p)
                                 (concat "File Moved & Renamed\n"
                                         "From: " old-filename "\n"
                                         "To:   " new-name))
                                (file-moved-p
                                 (concat "File Moved\n"
                                         "From: " old-filename "\n"
                                         "To:   " new-name))
                                (file-renamed-p
                                 (concat "File Renamed\n"
                                         "From: " old-short-name "\n"
                                         "To:   " new-short-name)))))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                old-short-name)
                               'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-buffer-name (read-string "New buffer name: ")))
                   (while (get-buffer new-buffer-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?")
                                  new-buffer-name))
                         (setq new-buffer-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-buffer-name)
                   (message (concat "Buffer Renamed\n"
                                    "From: " old-short-name "\n"
                                    "To:   " new-buffer-name))))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

;; (defun delete-current-buffer-file ()
;;   "Remove file connected to current buffer and kill buffer."
;;   (interactive)
;;   (let ((filename (buffer-file-name))
;;         (buffer (current-buffer))
;;         (name (buffer-name)))
;;     (if (not (and filename (file-exists-p filename)))
;;         (ido-kill-buffer)
;;       (if (yes-or-no-p
;;            (format "Are you sure you want to delete this file: '%s'?" name))
;;           (progn
;;             (delete-file filename t)
;;             (kill-buffer buffer)
;;             (message "File deleted: '%s'" filename))
;;         (message "Canceled: File deletion")))))

(defun spacemacs/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (if (yes-or-no-p
           (format "Are you sure you want to delete this file: '%s'?" name))
          (progn
            (delete-file filename t)
            (kill-buffer buffer)
            ;; (when (and (configuration-layer/package-used-p 'projectile)
            ;;            (projectile-project-p))
            ;;   (call-interactively #'projectile-invalidate-cache))
            (message "File deleted: '%s'" filename))
        (message "Canceled: File deletion")))))

(defun sudo-edit (&optional arg)
  "Edit file with administrator privileges."
  (interactive "P")
  (require 'tramp)
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (if (not (tramp-tramp-file-p fname))
         (concat "/sudo:root@localhost:" fname)
       (with-parsed-tramp-file-name fname parsed
         (when (equal parsed-user "root")
           (error "Already root!"))
         (let* ((new-hop (tramp-make-tramp-file-name
                          ;; Try to retrieve a tramp method suitable for
                          ;; multi-hopping
                          (cond ((tramp-get-method-parameter
                                  parsed 'tramp-login-program))
                                ((tramp-get-method-parameter
                                  parsed 'tramp-copy-program))
                                (t parsed-method))
                          parsed-user
                          parsed-domain
                          parsed-host
                          parsed-port
                          nil
                          parsed-hop))
                (new-hop (substring new-hop 1 -1))
                (new-hop (concat new-hop "|"))
                (new-fname (tramp-make-tramp-file-name
                            "sudo"
                            parsed-user
                            parsed-domain
                            parsed-host
                            parsed-port
                            parsed-localname
                            new-hop)))
           new-fname))))))

(defun open-file-in-external-app (file-path)
  "Open FILE-PATH in external application."
  (cond
   ((eq system-type 'windows-nt)
    (w32-shell-execute "open" (replace-regexp-in-string "/" "\\\\" file-path)))
   ((eq system-type 'darwin) (shell-command (format "open \"%s\"" file-path)))
   ((eq system-type 'gnu/linux) (let ((process-connection-type nil))
                                  (start-process "" nil "xdg-open" file-path)))))

(defun open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer."
  (interactive "P")
  (if arg
      (open-file-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (open-file-in-external-app file-path)
        (message "No file associated to this buffer.")))))


;;; Buffer

;; from https://gist.github.com/3402786
(defun toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (save-excursion
    (if (and (= 1 (length (window-list)))
             (assoc ?_ register-alist))
        (jump-to-register ?_)
      (progn
        (window-configuration-to-register ?_)
        (delete-other-windows)))))

(defun indent-region-or-buffer (&optional arg)
  "Indent a region if selected, otherwise the whole buffer.
if prefix argument ARG is given, `untabify' first."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (when arg
            (untabify (region-beginning) (region-end)))
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (when arg
          (untabify (region-beginning) (region-end)))
        (indent-region (point-min) (point-max))
        (message "Indented buffer.")))))

(defun switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (if arg
      (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
    (switch-to-buffer (get-buffer-create "*scratch*"))))

(defun switch-to-messages-buffer (&optional arg)
  "Switch to the `*Messages*' buffer in an other window.
if prefix argument ARG is given, switch to it directly."
  (interactive "P")
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (if arg
        (switch-to-buffer (current-buffer))
      (switch-to-buffer-other-window (current-buffer)))))

(defun switch-to-minibuffer-window ()
  "Switch to minibuffer window (if active)."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defvar killed-buffer-list nil
  "List of recently killed buffers.")

(defun add-buffer-to-killed-list ()
  "If buffer is associated with a file name, add that file
to the `killed-buffer-list' when killing the buffer."
  (when buffer-file-name
    (push buffer-file-name killed-buffer-list)))
(add-hook 'kill-buffer-hook #'add-buffer-to-killed-list)

(defun reopen-killed-buffer ()
  "Reopen the most recently killed file buffer, if one exists."
  (interactive)
  (when killed-buffer-list
    (find-file (pop killed-buffer-list))))

;; our own implementation of kill-this-buffer from menu-bar.el
(defun kill-current-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the current WINDOW."
  (interactive)
  (cl-destructuring-bind (buf start pos)
      (or (cl-find (window-buffer window) (window-prev-buffers)
                   :key #'car :test-not #'eq)
          (list (other-buffer) nil nil))
    (if (not buf)
        (message "Last buffer not found.")
      (set-window-buffer-start-and-point window buf start pos))))


;;; Window

;; from @bmag
(defun window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((window-tree (car (window-tree)))
             (current-split-vertical-p (car window-tree))
             (first-window (nth 2 window-tree))
             (second-window (nth 3 window-tree))
             (second-window-state (window-state-get second-window))
             (splitter (if current-split-vertical-p
                           #'split-window-horizontally
                         #'split-window-vertically)))
        (delete-other-windows first-window)
        ;; `window-state-put' also re-selects the window if needed, so we don't
        ;; need to call `select-window'
        (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two")))

;; originally from magnars and modified by ffevotte for dedicated windows
;; support, it has quite diverged by now
(defun rotate-windows-forward (count)
  "Rotate each window forwards.
A negative prefix argument rotates each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (let* ((non-dedicated-windows (cl-remove-if 'window-dedicated-p (window-list)))
         (states (mapcar #'window-state-get non-dedicated-windows))
         (num-windows (length non-dedicated-windows))
         (step (+ num-windows count)))
    (if (< num-windows 2)
        (error "You can't rotate a single window!")
      (dotimes (i num-windows)
        (window-state-put
         (elt states i)
         (elt non-dedicated-windows (% (+ step i) num-windows)))))))

(defun rotate-windows-backward (count)
  "Rotate each window backwards.
Dedicated (locked) windows are left untouched."
  (interactive "p")
  (rotate-windows-forward (* -1 count)))

;; from http://dfan.org/blog/2009/02/19/emacs-dedicated-windows/
;; (defun toggle-current-window-dedication ()
(defun spacemacs/toggle-current-window-dedication ()
  "Toggle dedication state of a window. Commands that change the buffer that a
window is displaying will not typically change the buffer displayed by
a dedicated window."
  (interactive)
  (let* ((window    (selected-window))
         (dedicated (window-dedicated-p window)))
    (set-window-dedicated-p window (not dedicated))
    (message "Window %sdedicated to %s"
             (if dedicated "no longer " "")
             (buffer-name))))

;; from https://gist.github.com/timcharper/493269
(defun split-window-vertically-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (split-window-vertically)
  (windmove-down))

(defun split-window-horizontally-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (split-window-horizontally)
  (windmove-right))

(defun alternate-window ()
  "Switch back and forth between current and last window in the current frame."
  (interactive)
  (let (;; switch to first window previously shown in this frame
        (prev-window (get-mru-window nil t t)))
    ;; Check window was not found successfully
    (unless prev-window (user-error "Last window not found"))
    (select-window prev-window)))


;;; Misc

(defun echo (msg &rest args)
  "Display MSG in echo-area without logging it in *Messages* buffer."
  (interactive)
  (let ((message-log-max nil))
    (apply 'message msg args)))

;; (defun ns-switch-back-to-previous-application ()
;;   "Switch back to previous application on macOS."
;;   (interactive)
;;   (do-applescript
;;    (mapconcat
;;     #'identity
;;     '("tell application \"System Events\""
;;       "  tell process \"Finder\""
;;       "    activate"
;;       "    keystroke tab using {command down}"
;;       "  end tell"
;;       "end tell")
;;     "\n")))
;; (defalias #'mac-switch-back-to-previous-application #'ns-switch-back-to-previous-application)

(defun set-file-executable ()
  "Add executable permissions on current file."
  (interactive)
  (when (buffer-file-name)
    (set-file-modes buffer-file-name
                    (logior (file-modes buffer-file-name) #o100))
    (message (concat "Made " buffer-file-name " executable"))))

;;; spacemacs

;; https://github.com/junghan0611/spacemacs/commit/9ce37c1ef68300f006bd2223e85cca836493a1f9
(defun spacemacs/compleseus-search-project ()
  "Search in current project."
  (interactive)
  (spacemacs/compleseus-search t (project-root (project-current t)))
  ;; (spacemacs/compleseus-search t (projectile-project-root))
  )

(defun spacemacs/compleseus-search-default ()
  "Search."
  (interactive)
  (spacemacs/compleseus-search-project))

(defun spacemacs/compleseus-switch-to-buffer ()
  "`consult-buffer' with buffers provided by persp."
  (interactive)
  (consult-buffer
   '(consult--source-hidden-buffer
     ;; consult--source-persp-buffers
     consult--source-modified-buffers
     consult--source-recent-file
     consult--source-bookmark
     consult--source-project-buffer
     consult--source-project-recent-file)))

(defun spacemacs/compleseus-search (use-initial-input initial-directory)
  (let* ((initial-input (if use-initial-input
                            (rxt-quote-pcre
                             (if (region-active-p)
                                 (buffer-substring-no-properties
                                  (region-beginning) (region-end))
                               (or (thing-at-point 'symbol t) "")))
                          ""))
         (default-directory
          (or initial-directory (read-directory-name "Start from directory: "))))
    (consult-ripgrep default-directory initial-input)))

(defun spacemacs/consult-line ()
  (interactive)
  (consult-line
   (if (region-active-p)
       (buffer-substring-no-properties
        (region-beginning) (region-end))
     (thing-at-point 'symbol t))))

(defun spacemacs/consult-line-multi ()
  (interactive)
  (consult-line-multi
   nil
   (if (region-active-p)
       (buffer-substring-no-properties
        (region-beginning) (region-end))
     (thing-at-point 'symbol t))))

(defun consult-line-symbol ()
  (interactive)
  (consult-line
   (if (region-active-p)
       (buffer-substring-no-properties
        (region-beginning) (region-end))
     (thing-at-point 'symbol t))))

(defun spacemacs/compleseus-search-auto ()
  "Choose folder to search."
  (interactive)
  (spacemacs/compleseus-search t nil))

(defun spacemacs/compleseus-search-dir ()
  "Search current folder."
  (interactive)
  (spacemacs/compleseus-search t default-directory))

;;;###autoload
(defun my/compleseus-search-dir ()
  "Search current folder with no initial input"
  (interactive)
  (spacemacs/compleseus-search nil default-directory))

(defun my/compleseus-search-auto-hidden ()
  "Search folder with hiddens files"
  (interactive)
  (let*
      ((initial-directory (read-directory-name "Start from directory: "))
       (consult-ripgrep-args
        (concat "rg "
                "--null "
                "-. "  ;; for dotfiles e.g. .spacemacs.el
                "--line-buffered "
                "--color=never "
                "--line-number "
                "--smart-case "
                "--no-heading "
                "--max-columns=1000 "
                "--max-columns-preview "
                "--with-filename "
                (shell-quote-argument initial-directory))))
    (consult-ripgrep)))

;; our own implementation of kill-this-buffer from menu-bar.el
(defun spacemacs/kill-this-buffer (&optional arg)
  "Kill the current buffer.
If the universal prefix argument is used then kill also the window."
  (interactive "P")
  (if (window-minibuffer-p)
      (abort-recursive-edit)
    (if (equal '(4) arg)
        (kill-buffer-and-window)
      (kill-buffer))))

(defun spacemacs/compleseus-search-from (input)
  "Embark action to start ripgrep search from candidate's directory."
  (interactive "s")
  (message "The first input %s." input)
  (let ((dir (if (file-directory-p input)
                 input
               (file-name-directory input))))
    (consult-ripgrep dir)))

(defun spacemacs/compleseus-find-file ()
  "Calls the interactive find-file browser.
This solves the problem: Binding a key to: `find-file' calls: `ido-find-file'"
  (interactive)
  (call-interactively 'find-file))

;; from magnars
(defun spacemacs/sudo-edit (&optional arg)
  (interactive "P")
  (require 'tramp)
  (let ((fname (if (or arg (not buffer-file-name))
                   (read-file-name "File: ")
                 buffer-file-name)))
    (find-file
     (if (not (tramp-tramp-file-p fname))
         (concat "/sudo:root@localhost:" fname)
       (with-parsed-tramp-file-name fname parsed
         (when (equal parsed-user "root")
           (error "Already root!"))
         (let* ((new-hop (tramp-make-tramp-file-name
                          ;; Try to retrieve a tramp method suitable for
                          ;; multi-hopping
                          (cond ((tramp-get-method-parameter
                                  parsed 'tramp-login-program))
                                ((tramp-get-method-parameter
                                  parsed 'tramp-copy-program))
                                (t parsed-method))
                          parsed-user
                          parsed-domain
                          parsed-host
                          parsed-port
                          nil
                          parsed-hop))
                (new-hop (substring new-hop 1 -1))
                (new-hop (concat new-hop "|"))
                (new-fname (tramp-make-tramp-file-name
                            "sudo"
                            parsed-user
                            parsed-domain
                            parsed-host
                            parsed-port
                            parsed-localname
                            new-hop)))
           new-fname))))))

(defun spacemacs/delete-file (filename &optional ask-user)
  "Remove specified file or directory.

Also kills associated buffer (if any exists) and invalidates
projectile cache when it's possible.

When ASK-USER is non-nil, user will be asked to confirm file
removal."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename)
      ;; (when (and (configuration-layer/package-used-p 'projectile)
      ;;            (projectile-project-p))
      ;;   (call-interactively #'projectile-invalidate-cache))
      )))

;;; jump-out-of-pair

  ;;;###autoload
(defun jump-out-of-pair ()
  (interactive)
  (let ((found (search-forward-regexp "[])}\"'`*=]" nil t)))
    (when found
      (cond ((or (looking-back "\\*\\*" 2)
  		 (looking-back "``" 2)
  		 (looking-back "\"\"" 2) ; 2023-10-02 added
  		 (looking-back "''" 2)
  		 (looking-back "==" 2))
  	     (forward-char))
  	    (t (forward-char 0))))))

  ;;;###autoload
(defun jump-backward-pair ()
  (interactive)
  (let ((found (search-backward-regexp "[])}\"'`*=]" nil t)))
    (when found
      (cond ((or (looking-back "\\*\\*" 2)
                 (looking-back "``" 2)
                 (looking-back "\"\"" 2) ; 2023-10-02 added
                 (looking-back "''" 2)
                 (looking-back "==" 2))
             (backward-char))
            (t (backward-char 0))))))

;;; corgi

;; /corgi-packages/corgi-commands/corgi-commands.el:17
(require 'seq)

(defun corgi/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun corgi/switch-to-last-elisp-buffer ()
  (interactive)
  (when-let ((the-buf (seq-find (lambda (b)
                                  (with-current-buffer b
                                    (derived-mode-p 'emacs-lisp-mode)))
                                (buffer-list))))
    (pop-to-buffer the-buf)))

(defun corgi/double-columns ()
  "Simplified version of spacemacs/window-split-double-column"
  (interactive)
  (delete-other-windows)
  (let* ((previous-files (seq-filter #'buffer-file-name
                                     (delq (current-buffer) (buffer-list)))))
    (set-window-buffer (split-window-right)
                       (or (car previous-files) "*scratch*"))
    (balance-windows)))

(defun corgi/open-init-el ()
  "Open the user's init.el file"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun corgi/-locate-file (fname)
  "Like corkey/-locate-file"
  (cond
   ((symbolp fname)
    (corgi/-locate-file (concat (symbol-name fname) ".el")))
   ((file-exists-p (expand-file-name fname user-emacs-directory))
    (expand-file-name fname user-emacs-directory))
   (t (locate-library fname))))

(defun corgi/open-keys-file ()
  "Open the Corgi built-in key binding file"
  (interactive)
  (find-file (corgi/-locate-file 'corgi-keys)))

(defun corgi/open-signals-file ()
  "Open the Corgi built-in signals file"
  (interactive)
  (find-file (corgi/-locate-file 'corgi-signals)))

(defun corgi/open-user-keys-file ()
  "Open the user key binding file in the emacs user directory
Will create one if it doesn't exist."
  (interactive)
  (let ((keys-file (corgi/-locate-file 'user-keys)))
    (when (not keys-file)
      (copy-file (corgi/-locate-file 'user-keys-template)
                 (expand-file-name "user-keys.el" user-emacs-directory)))
    (find-file (corgi/-locate-file 'user-keys))))

(defun corgi/open-user-signals-file ()
  "Open the user signal file in the emacs user directory
Will create one if it doesn't exist."
  (interactive)
  (let ((signals-file (corgi/-locate-file 'user-signals)))
    (when (not signals-file)
      (copy-file (corgi/-locate-file 'user-signals-template)
                 (expand-file-name "user-signals.el" user-emacs-directory)))
    (find-file (corgi/-locate-file 'user-signals))))

;; Taking this out, see explanation in corgi-keys.el
;;
;; (defun corgi/emulate-tab ()
;;   "Emulates pressing <tab>, used for binding to TAB for tab key
;; support in terminals."
;;   (interactive)
;;   (let ((cmd (key-binding (kbd "<tab>"))))
;;     (when (commandp cmd)
;;       (call-interactively cmd))))


(provide 'core-funcs)
;;; embark

;; vertico
(defun spacemacs/embark-preview ()
  "Previews candidate in vertico buffer, unless it's a consult command"
  (interactive)
  (unless (bound-and-true-p consult--preview-function)
    (save-selected-window
      (let ((embark-quit-after-action nil))
        (embark-dwim)))))

(defun spacemacs/next-candidate-preview (&optional n)
  "Go forward N candidates and preview"
  (interactive)
  (vertico-next (or n 1))
  (spacemacs/embark-preview))

(defun spacemacs/previous-candidate-preview (&optional n)
  "Go backward N candidates and preview"
  (interactive)
  (vertico-previous (or n 1))
  (spacemacs/embark-preview))

;; which-key integration functions for embark
;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
(defun spacemacs/embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(defun spacemacs/embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'spacemacs/embark-which-key-indicator embark-indicators)))
    (apply fn args)))

(defun spacemacs/embark-action-completing-read ()
  "Bypass `embark-act' and execute `embark-keymap-help' directly.

This function mimics the Helm action menu.
Note: this function relies on embark internals and might break upon embark updates.
"
  (interactive)
  (require 'embark)
  (let* ((targets (or (embark--targets) (user-error "No target found")))
         (indicators (mapcar #'funcall embark-indicators))
         (default-done nil))
    (unwind-protect
        (while
            (let* ((target (car targets))
                   (action (embark-completing-read-prompter
                            (let ((embark-default-action-overrides
                                   (if default-done
                                       `((t . ,default-done))
                                     embark-default-action-overrides)))
                              (embark--action-keymap (plist-get target :type)
                                                     (cdr targets)))
                            nil))
                   (default-action (or default-done
                                       (embark--default-action
                                        (plist-get target :type)))))
              ;; if the action is non-repeatable, cleanup indicator now
              (mapc #'funcall indicators)
              (condition-case err
                  (embark--act
                   action
                   (if (and (eq action default-action)
                            (eq action embark--command)
                            (not (memq action embark-multitarget-actions)))
                       (embark--orig-target target)
                     target)
                   (embark--quit-p action))
                (user-error
                 (funcall (if repeat #'message #'user-error)
                          "%s" (cadr err))))))
      (mapc #'funcall indicators))))
;;; core-funcs.el ends here
