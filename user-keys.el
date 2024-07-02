;;; -*- no-byte-compile: t -*-

;; This is your user keys file, here you can configure key bindings that will
;; get added to Corgi. You can also override Corgi's default bindings this way.
;;
;; Bindings here are nested, e.g. `("SPC" ("b" ("k" kill-buffer)))' means that
;; "space" followed by "b" and then "k" will invoke `M-x kill-buffer'.
;;
;; You can add a descriptions before the command, this will show up in a pop-up
;; when you press the prefix key and wait a bit. (This uses which-key)
;;
;; `("SPC" ("b" ("k" "Choose a buffer to kill" kill-buffer)))'
;;
;; Instead of a prefix key you can use a symbol like `normal' or `insert', which
;; designates the Evil state (what vim calls the mode). `global' means any
;; state, `normal|visual' means either normal or visual.
;;
;; Instead of a command like `kill-buffer' you can put a keyword like
;; `:eval/buffer'. This is called a "signal". In the `corgi-signals' (or
;; `user-signals') file these are bound to specific commands based on the major
;; mode. E.g. in Emacs Lisp `:eval/buffer' means `eval-buffer', whereas in
;; Clojure it means `cider-eval-buffer'.

(bindings
 ;; "global" bindings are always active regardless of Evil's "state" (= vim mode)
 ;; If you don't provide this the default is `normal'.
 (global
  )

 ;; Bindings for commands are usually only active in normal and visual state.
 (normal|visual
  ("SPC"
   ("!" "shell cmd" shell-command)
   ("b" "Buffer commands"
    ("m" "switch-to-messages-buffer" switch-to-messages-buffer)
    ("w" "Toggle read-only" read-only-mode))
   ("s" "Search commands"
    ("s" "consult-line" consult-line)
    ("S" "consult-line-symbol" consult-line-symbol)
    ("g" "consult-grep" consult-grep)
    ("d" "my/compleseus-search-dir" my/compleseus-search-dir)
    ("D" "spacemacs/compleseus-search-dir" spacemacs/compleseus-search-dir)
    ("f" "spacemacs/compleseus-search-auto" spacemacs/compleseus-search-auto)
    ("F" "my/compleseus-search-auto-hidden" my/compleseus/search-auto-hidden))
   ("g" "Git"
    ("c" "evilnc-comment-operator" evilnc-comment-operator)
    ("b" "Git blame" magit-blame))
   ("o" "Open"
    ("u" "Open URL at point" browse-url-at-point)
    ("s" "Edit string at point" string-edit-at-point))
   ;; ("0" "Select Treemacs" treemacs-select-window)
   ;; ("f"
   ;;  ("t" "Turn Treemacs on/off" treemacs)
   ;;  ("T" "Focus current file in file tree" treemacs-find-file))
   ("w" "Windows"
    ("r" "Rotate / swap windows" window-swap-states)
    ("s" "Rotate / swap windows" window-swap-states)
    )
   ("k" "+lisp"
    ("b" "forward-barf" puni-barf-forward)
    ("c" "convolute" puni-convolute)
    ("s" "forward-slurp" puni-slurp-forward)
    ("t" "transpose" puni-transpose)
    )
   )
  ("<backspace>" "Switch to previous buffer" corgi/switch-to-previous-buffer)
  ("." "consult-line" consult-line) ; 이게 편함.
  ) ;; end of (normal|visual)
 )
