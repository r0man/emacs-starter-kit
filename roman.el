
;;; Custom elpa packages.
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

;;; Install the custom elpa packages, if not already installed.
(starter-kit-elpa-install)

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

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

;; Use my custom color theme.
(require 'color-theme)
(load-file "~/.emacs.d/color-theme-roman.el")
(color-theme-roman)

;; Start maximized.
                                        ;(toggle-fullscreen)

;; Show the menu-bar, but not the scroll-bar.
(if (fboundp 'menu-bar-mode) (menu-bar-mode t))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Enable cut-and-paste between Emacs and X clipboard.
(setq x-select-enable-clipboard t)

;;; COMPILE-MODE
(setq compilation-scroll-output 't)

;; CLOJURE-MODE
(defun clojure-mode-setup-indent ()
  (define-clojure-indent (are 1))
  (define-clojure-indent (dstest 1))
  (define-clojure-indent (uncountable 1)))

(add-hook 'clojure-mode-hook 'clojure-mode-setup-indent)

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

;;; ESPRESSO MODE
(setq auto-mode-alist (remove '("\\.js$" . espresso-mode) auto-mode-alist))
(setq auto-mode-alist (remove '("\\.json$" . espresso-mode) auto-mode-alist))

;;; FLYSPELL MODE.
(dolist (hook '(LaTeX-mode-hook))
  (add-hook hook 'flyspell-mode))

(setq flyspell-abbrev-p t) ; Add corrected words to abbreviation
; table.

;;; GIT
(add-to-list 'load-path "/usr/share/doc/git-core/contrib/emacs") 
(require 'git-blame)

;;; HIPPIE EXPAND
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        ))

;;; MOZ-REPL MODE
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

;;; SMART-TAB
(setq smart-tab-using-hippie-expand t)
(dolist (hook '(emacs-lisp-mode-hook
                haml-mode-hook
                LaTeX-mode-hook
                ruby-mode-hook
                yaml-mode
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

;;; RUBY-TEST MODE
;; (require 'ruby-test-mode)

;;; EMACS RAILS RELOADED
(setq load-path (cons (expand-file-name "~/.emacs.d/emacs-rails-reloaded") load-path))
(require 'rails-autoload)

(defun switch-to-rails-runner-buffer ()
  (switch-to-buffer-other-window rails/runner/buffer-name)
  (other-window -1))

(defadvice rails/compile/current-method (after rails/compile/current-method-advice) ()
  "Switch to the rails runner buffer after running the method test."
  (switch-to-rails-runner-buffer))

(ad-activate 'rails/compile/current-method)

(defadvice rails/compile/single-file (after rails/compile/single-file-advice) ()
  "Switch to the rails runner buffer after running the file test."
  (switch-to-rails-runner-buffer))

(ad-activate 'rails/compile/single-file)

;;; SHELL MODE
(require 'shell)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(define-key shell-mode-map (kbd "M-r") 'ido-complete-comint-input-ring)
(define-key shell-mode-map (kbd "M-s") 'ido-complete-comint-input-ring)

;;; KEY BINDINGS

;; Restore some default key bindings overridden by emacs starter kit.
(global-set-key (kbd "C-x h") 'mark-whole-buffer)
(global-set-key (kbd "C-x ^") 'enlarge-window)
(global-set-key (kbd "C-x C-o") 'delete-blank-lines)

;; Unset the eshell ey bindings.
(global-unset-key (kbd "C-x m"))
(global-unset-key (kbd "C-x M"))

;; Cycle through or spawn new shell buffers.
(global-set-key [f5] 'compile)
(global-set-key [f7] 'shell-dwim)
(global-set-key (kbd "C-x I") 'indent-buffer)
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

