
;;; Also install my custom packages from elpa ...
(setq starter-kit-packages
      (list
       'clojure-mode
       'clojure-test-mode
       'css-mode
       'erc
       'gist
       'haml-mode
       'idle-highlight
       'inf-ruby
       'js2-mode
       'json
       'magit
       'ruby-mode
       'sass-mode
       'smart-tab
       'yaml-mode
       ))

(starter-kit-elpa-install)

(defun google (query)
  "Search for QUERY on Google."
  (interactive "sGoogle: ")
  (browse-url (concat "http://www.google.com/search?q=" query)))

(defun toggle-fullscreen ()
  "Toggle fullscreen mode."
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))

;; Invoking multiple shells, and defining new shells.
;; See: http://www.emacswiki.org/emacs/ShellMode#toc3
(defun shell-dwim (&optional create)
  "Start or switch to an inferior shell process, in a smart way.
 If a buffer with a running shell process exists, simply switch to
 that buffer.
 If a shell buffer exists, but the shell process is not running,
 restart the shell.
 If already in an active shell buffer, switch to the next one, if
 any.
 With prefix argument CREATE always start a new shell."
  (interactive "P")
  (let* ((next-shell-buffer
          (catch 'found
            (dolist (buffer (reverse (buffer-list)))
              (when (and
                     (string-match "^\\*shell\\*" (buffer-name buffer))
                     (not (string= (buffer-name) (buffer-name buffer))))
                (throw 'found buffer)))))
         (buffer (if create (generate-new-buffer-name "*shell*")
                   next-shell-buffer)))
    (shell buffer)))

;; Use IDO fro comint history
;; See: http://paste.lisp.org/display/37129 (modified to remove duplicate)
(defun ido-complete-comint-input-ring ()
  "Fetch a previous element from history using ido-like completion.
This function searches *all* elements in history, not just
previous or next elements like Comint's normal completion.
So you can bind it to both M-r and M-s."
  (interactive)
  (unless (null comint-input-ring)
    (let* ((elt (ido-completing-read
		 "History: " (delete "" (remove-duplicates (cddr (ring-elements comint-input-ring)) :test #'string=))
		 nil t))
	   (pos (comint-previous-matching-input-string-position
		 (regexp-quote elt) 1)))
      (unless (null pos)
	(setq comint-input-ring-index pos)
	(message "History item: %d" (1+ pos))
	(comint-delete-input)
	(insert (ring-ref comint-input-ring pos))))))

;; Use ido for the shell history.
(require 'shell)
(define-key shell-mode-map (kbd "M-r") 'ido-complete-comint-input-ring)
(define-key shell-mode-map (kbd "M-s") 'ido-complete-comint-input-ring)

(require 'color-theme)
(load-file "~/.emacs.d/color-theme-roman.el")
(color-theme-roman)

;;; Start emacs maximized.
(toggle-fullscreen)

;;; COMPILE-MODE
(setq compilation-scroll-output 't)

;; DESKTOP SAVE MODE
(setq desktop-path '("." "~" "~/.emacs.d"))
(setq desktop-save 'if-exists)
(setq desktop-globals-to-save
      (append '((extended-command-history . 30)
                (file-name-history        . 100)
                (grep-history             . 30)
                (compile-history          . 30)
                (minibuffer-history       . 50)
                (query-replace-history    . 60)
                (read-expression-history  . 60)
                (regexp-history           . 60)
                (regexp-search-ring       . 20)
                (search-ring              . 20)
                (shell-command-history    . 250)
                tags-file-name
                register-alist)))
(desktop-save-mode 1)

;;; ERC MODE
(setq erc-nick "r0man")
(setq erc-user-full-name "Roman Scherer")

(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist '(("freenode.net" "#clojure")))

;;; FLYSPELL MODE.
(dolist (hook '(LaTeX-mode-hook))
  (add-hook hook 'flyspell-mode))
(setq flyspell-abbrev-p t) ; Add corrected words to abbreviation table.

;;; HIPPIE EXPAND
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        ))

;;; MENU BAR MODE
(if (fboundp 'menu-bar-mode) (menu-bar-mode t))

;;; SMART-TAB
(setq smart-tab-using-hippie-expand t)
(dolist (hook '(emacs-lisp-mode-hook
                haml-mode-hook
                LaTeX-mode-hook
                ruby-mode-hook
                slime-mode-hook))
  (add-hook hook (lambda () (smart-tab-mode t))))

;;; COMINT MODE
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 )

(ansi-color-for-comint-mode-on) ; Interpret and use ansi color codes in shell buffers

;;; ZSH SHELL HISTORY
(if (string-match ".*/zsh$" (getenv "SHELL"))
    (setenv "HISTFILE" "~/.histfile"))

;;; KEY BINDINGS

;; Restore some default key bindings overridden by emacs starter kit.
(global-set-key (kbd "C-x h") 'mark-whole-buffer)
(global-set-key (kbd "C-x ^") 'enlarge-window)
(global-set-key (kbd "C-c s") 'magit-status)
(global-set-key (kbd "C-x C-o") 'delete-blank-lines)

;; Unset the eshell ey bindings.
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "C-x M"))

;; Cycle through or spawn new shell buffers.
(global-set-key [f7] 'shell-dwim)

;; Compile with F5.
(global-set-key [f5] 'compile)
