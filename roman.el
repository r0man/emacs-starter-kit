
;; Use the Marmalade repository as well.
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;;; Custom elpa packages.
(setq starter-kit-packages
      (list
       'clojure-mode
       'clojure-test-mode
       'css-mode
       'gist
       'haml-mode
       'idle-highlight
       'inf-ruby
       'json
       'ruby-mode
       'ruby-test-mode
       'sass-mode
       'slime
       'slime-repl
       'smart-tab
       'yaml-mode
       'yasnippet-bundle
       ))

;;; Install the custom elpa packages, if not already installed.
(starter-kit-elpa-install)

;; Hadoop Home
(setenv "HADOOP_HOME" (expand-file-name (concat (getenv "HOME") "/local/hadoop")))

(setq
 exec-path
 (list
  (expand-file-name "~/bin")
  (concat (getenv "HADOOP_HOME") "/bin")
  (concat (getenv "HADOOP_HOME") "/src/contrib/ec2/bin")
  "/usr/local/sbin"
  "/usr/local/bin"
  "/usr/sbin"
  "/usr/bin"
  "/sbin"
  "/bin"
  "/usr/games"))

;;; Build PATH from exec-path.
(setenv "PATH" (mapconcat 'identity exec-path ":"))
(setenv "JAVA_HOME" "/usr/lib/jvm/java-6-openjdk")

;; DELETE TRAILING WHITESPACE
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; AMAZON WEB SERVICES
(let ((aws-credentials (expand-file-name "~/.aws.el")))
  (if (file-exists-p aws-credentials)
      (progn
        (load-file aws-credentials)
        (setenv "AWS_ACCOUNT_NUMBER" aws-account-number)
        (setenv "AWS_ACCESS_KEY_ID" aws-access-key-id)
        (setenv "AWS_SECRET_ACCESS_KEY" aws-secret-access-key)
        (setenv "EC2_PRIVATE_KEY" (expand-file-name ec2-private-key))
        (setenv "EC2_CERT" (expand-file-name ec2-cert))
        (setenv "S3_ACCESS_KEY" aws-access-key-id)
        (setenv "S3_SECRET_KEY" aws-secret-access-key))))

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
    (let* ((elt (ido-completing-read "History: " (delete "" (remove-duplicates (cddr (ring-elements comint-input-ring)) :test #'string=)) nil t))
           (pos (comint-previous-matching-input-string-position
                 (regexp-quote elt) 1)))
      (unless (null pos)
        (setq comint-input-ring-index pos)
        (message "History item: %d" (1+ pos))
        (comint-delete-input)
        (insert (ring-ref comint-input-ring pos))))))

;; Show the menu-bar, but not the scroll-bar.
(if (fboundp 'menu-bar-mode) (menu-bar-mode t))

;; Use my custom color theme.
(require 'color-theme)
(load-file "~/.emacs.d/color-theme-roman.el")
(color-theme-roman)

;; Highlight trailing whitespace
(setq show-trailing-whitespace t)

;; Enable cut-and-paste between Emacs and X clipboard.
(setq x-select-enable-clipboard t)

;; Controls the operation of the TAB key.
(setq tab-always-indent 'complete)

;; Do not add a final newline when saving.
(setq require-final-newline nil)

;; AUTO-COMPLETE
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; AC-SLIME
(require 'slime)
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

;;; COMPILE-MODE
(setq compilation-scroll-output 't)

;; CLOJURE-MODE
(defun define-clojure-indent-words ()
  (define-clojure-indent (api-test 1))
  (define-clojure-indent (api-admin-test 1))
  (define-clojure-indent (admin-db-test 1))
  (define-clojure-indent (analytic-db-test 1))
  (define-clojure-indent (are 1))
  (define-clojure-indent (controller-test 1))
  (define-clojure-indent (context 1))
  (define-clojure-indent (database-test 1))
  (define-clojure-indent (datastore-test 1))
  (define-clojure-indent (dbtest 1))
  (define-clojure-indent (emits-once 1))
  (define-clojure-indent (ensure-open 1))
  (define-clojure-indent (expect 1))
  (define-clojure-indent (memcache-test 1))
  (define-clojure-indent (task-queue-test 1))
  (define-clojure-indent (uncountable 1))
  (define-clojure-indent (user-test 1)))

(add-hook 'clojure-mode-hook 'define-clojure-indent-words)

(progn
  (defun nuggad-test-maybe-enable ()
    "Enable clojure-test-mode if the current buffer contains nuggad.test."
    (save-excursion
      (save-window-excursion
        (goto-char (point-min))
        (when (search-forward "nuggad.test" nil t)
          (clojure-test-mode t)))))
  (add-hook 'clojure-mode-hook 'nuggad-test-maybe-enable))

;; CLOSURE-LINT-MODE
(require 'closure-lint-mode)

;; DURENDAL - https://github.com/technomancy/durendal
(require 'durendal)

;; ELEIN
(require 'elein)

;;; GIST
(setq gist-view-gist t)

;; JAVA
(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)

;;; RVM
(require 'rvm)
(set 'rvm-executable (if (file-exists-p "~/.rvm/bin/rvm") "~/.rvm/bin/rvm" "/usr/local/bin/rvm"))
(rvm-use-default)
(setenv "rvm_path" "/usr/local/rvm")

;;; ESHELL
(require 'eshell-ext)

;;; FLYSPELL MODE.
(dolist (hook '(LaTeX-mode-hook)) (add-hook hook 'flyspell-mode))
(setq flyspell-abbrev-p t)

;; FLYMAKE (fix the annoying dialog)
(require 'flymake)
(defun flymake-report-fatal-status (status warning)
  "Display a warning and switch flymake mode off."
  (message (format "Flymake: %s. Flymake will be switched OFF" warning))
  (flymake-mode 0))

;;; GIT
(require 'magit)
(dolist (filename '("/usr/share/doc/git/contrib/emacs/git-blame.el"
                    "/usr/share/emacs/site-lisp/git/git-blame.el"))
  (if (file-exists-p filename) (load-file filename)))

;; HASKELL-MODE
(require 'haskell-mode)

;;; HIPPIE EXPAND
(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name))

;;; RCIRC
(require 'rcirc-color)
(require 'rcirc-late-fix)
(require 'rcirc-notify)
(if (file-exists-p "~/.rcirc.el") (load-file "~/.rcirc.el"))
(setq rcirc-default-nick "r0man"
      rcirc-default-user-name "r0man"
      rcirc-default-full-name "Roman Scherer"
      rcirc-server-alist '(("irc.freenode.net" :channels ("#clojure" "#clojureql" "#pallet")))
      rcirc-private-chat t
      rcirc-debug-flag t)
(add-hook 'rcirc-mode-hook
          (lambda ()
            (set (make-local-variable 'scroll-conservatively) 8192)
            (rcirc-track-minor-mode 1)
            (flyspell-mode 1)))

;;; SASS
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

;;; SMART-TAB
(setq smart-tab-using-hippie-expand t)
(dolist (hook '(c-mode-hook
                emacs-lisp-mode-hook
                haml-mode-hook
                html-mode-hook
                java-mode-hook
                LaTeX-mode-hook
                ruby-mode-hook
                yaml-mode-hook
                css-mode-hook
                paredit-mode
                slime-mode-hook))
  (add-hook hook (lambda () (smart-tab-mode t))))

;; SOY-MODE
(require 'soy-mode)

;;; TRAMP
(require 'tramp)
(tramp-set-completion-function
 "ssh"
 '((tramp-parse-shosts "~/.ssh/known_hosts")
   (tramp-parse-hosts "/etc/hosts")))

;;; COMINT MODE
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 )

;;; RE-BUILDER (PERL)
(require 're-builder-x)

;;; RUBY-TEST MODE
(require 'ruby-test-mode)
(setq ruby-test-ruby-executables '("/usr/local/rvm/rubies/ruby-1.9.2-p136/bin/ruby")
      ruby-test-rspec-executables '("/usr/local/rvm/gems/ruby-1.9.2-p136/bin/rspec"))

;; SQL-MODE
(let ((filename "~/.sql.el")) (if (file-exists-p filename) (load-file filename)))

;; ;;; EMACS RAILS RELOADED
(setq load-path (cons (expand-file-name "~/.emacs.d/emacs-rails-reloaded") load-path))
(setq rails/webserver-bundle/default-type "webrick")
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

;;; KEY BINDINGS

;; Restore some default key bindings overridden by emacs starter kit.
(global-set-key (kbd "C-x h") 'mark-whole-buffer)
(global-set-key (kbd "C-x ^") 'enlarge-window)
(global-set-key (kbd "C-x C-o") 'delete-blank-lines)
(global-unset-key (kbd "C-x g"))

(global-set-key (kbd "C-c C-n") 'durendal-sort-ns)
(global-set-key (kbd "C-c C-s") 'swap-windows)
(global-set-key (kbd "C-x C-g b") 'mo-git-blame-current)
(global-set-key (kbd "C-x C-g s") 'magit-status)
(global-set-key (kbd "C-x I") 'indent-buffer)
(global-set-key (kbd "C-x TAB") 'indent-rigidly)
(global-set-key [f11] 'fullscreen)
(global-set-key [f5] 'compile)

;; Jump into eshell
(eshell)
