
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
       'json
       'magit
       'ruby-mode
       'ruby-test-mode
       'sass-mode
       'smart-tab
       'yaml-mode
       ))

;;; Install the custom elpa packages, if not already installed.
(starter-kit-elpa-install)

(setq
 exec-path
 '(
   "~/bin"
   "/usr/local/rvm/rubies/ruby-1.9.1-p378/bin"
   "/usr/local/rvm/gems/ruby-1.9.1-p378/bin"
   "/usr/local/rvm/gems/ruby-1.9.1-p378%global/bin"
   "/usr/local/rvm/bin"
   "/usr/local/sbin"
   "/usr/local/bin"
   "/usr/sbin"
   "/usr/bin"
   "/sbin"
   "/bin"
   "/usr/games"))

(require 'rvm)

;;; Build PATH from exec-path.
(setenv "PATH" (mapconcat 'identity exec-path ":"))

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\\n]*\\|[[:space:]\\n]*$\\)" "" s)))

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

(defun fullscreen (&optional f)
  (interactive)
  (set-frame-parameter
   f 'fullscreen
   (if (frame-parameter f 'fullscreen) nil 'fullboth)))

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

(defun swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((/= (count-windows) 2)
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1))))
  (other-window 1))

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

;; Show the menu-bar, but not the scroll-bar.
(if (fboundp 'menu-bar-mode) (menu-bar-mode t))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Enable cut-and-paste between Emacs and X clipboard.
(setq x-select-enable-clipboard t)

;; Insidious Big Brother Database
(setq bbdb-file "~/.emacs.d/bbdb") 
(require 'bbdb)
(bbdb-initialize 'gnus)
(setq bbbd-message-caching-enabled t  
      bbdb-always-add-address t           
      bbdb-canonicalize-redundant-nets-p t
      bbdb-complete-name-allow-cycling t
      bbdb-completion-type nil          
      bbdb-default-area-code 49
      bbdb-north-american-phone-numbers-p nil
      bbdb-dwim-net-address-allow-redundancy t
      bbdb-electric-p t
      bbdb-elided-display t           
      bbdb-ignore-some-messages-alist '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter"))
      bbdb-offer-save 1
      bbdb-popup-target-lines 1              
      bbdb-quiet-about-name-mismatches 2  
      bbdb-use-alternate-names t      
      bbdb-use-pop-up t
      bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook)

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
(setq erc-autojoin-channels-alist '(("freenode.net" "#soundcloud")))
(require 'erc-customize)

;;; FORTUNE 
(setq fortune-file "~/.emacs.d/fortune-chucknorris")

;;; FLYSPELL MODE.
(dolist (hook '(LaTeX-mode-hook))
  (add-hook hook 'flyspell-mode))

(setq flyspell-abbrev-p t) ; Add corrected words to abbreviation ; table.

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

;;; IBUFFER-GIT
(add-to-list 'load-path "~/.emacs.d/ibuffer-git")
(require 'ibuffer-git)
(setq ibuffer-formats
      '((mark modified read-only git-status-mini
              " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (git-status 8 8 :left)
              " "
              filename-and-process)
        (mark " " (name 16 -1) " " filename)))

;;; MOZ-REPL MODE
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(defun javascript-custom-setup ()
  (moz-minor-mode 1))

;;; SMART-TAB
(setq smart-tab-using-hippie-expand t)
(dolist (hook '(emacs-lisp-mode-hook
                haml-mode-hook
                html-mode-hook
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
(global-set-key [f11] 'fullscreen)
(global-set-key (kbd "C-x I") 'indent-buffer)
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-c s") 'swap-windows)

