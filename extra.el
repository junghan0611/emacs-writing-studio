;;; extra.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Junghan Kim
;;
;; Author: Junghan Kim <junghanacs@gmail.com>
;; Maintainer: Junghan Kim <junghanacs@gmail.com>
;; Created: May 02, 2024
;; Modified: May 02, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/junghan0611/extra
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;;;; Hangul Korean

(setq default-input-method "korean-hangul")
(set-language-environment "Korean")
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system  'utf-8)
(prefer-coding-system 'utf-8)
(set-charset-priority 'unicode)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

(set-selection-coding-system 'utf-8) ;; important

;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(setq-default line-spacing 3) ; use fontaine

;; (setenv "LANG" "en_US.UTF-8")
;; (setenv "LC_ALL" "en_US.UTF-8")
;; (setenv "LANG" "ko_KR.UTF-8")

;; 날짜 표시를 영어로한다. org mode에서 time stamp 날짜에 영향을 준다.
(setq system-time-locale "C")

(setq input-method-verbose-flag nil
      input-method-highlight-flag nil)

(global-set-key (kbd "<S-SPC>") 'toggle-input-method)
;; (global-set-key (kbd "<Alt_R>") 'toggle-input-method)
(global-set-key (kbd "<Hangul>") 'toggle-input-method)
;; (global-unset-key (kbd "S-SPC"))

;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; +------------+------------+
;; | ABCDEFGHIJ | ABCDEFGHIJ |
;; +------------+------------+
;; | 1234567890 | 1234567890 |
;; +------------+------------+
;; | 일이삼사오 | 일이삼사오 |
;; | abcdefghij | abcdefghij |
;; +------------+------------+


(when (display-graphic-p)
  (set-face-attribute 'default nil :family "Monoplex KR Nerd" :width 'normal :weight 'regular :height 140)
  (set-fontset-font nil 'hangul (font-spec :family "Monoplex KR Nerd"))
  ;; (set-face-attribute 'fixed-pitch nil :family "Sarasa Term K" :width 'normal :weight 'regular)
  ;; (set-face-attribute 'fixed-pitch-serif nil :family "Hahmlet" :width 'normal :weight 'regular)
  ;; (set-face-attribute 'variable-pitch nil :family "Pretendard Variable"
  ;;                     :width 'normal :weight 'regular)

  (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil)
  (set-fontset-font t 'emoji (font-spec :family "Noto Emoji") nil 'prepend) ; Top

  (set-fontset-font t 'symbol (font-spec :family "Symbola") nil)
  (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols 2") nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family "Noto Sans Symbols") nil 'prepend)
  )

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun load-theme@run-hooks (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add 'load-theme :after #'load-theme@run-hooks)

(defun load-theme@theme-dont-propagate (&rest _)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))
(advice-add #'load-theme :before #'load-theme@theme-dont-propagate)

;;; extra.el ends here
