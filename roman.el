
;; Set ELPA package archives.
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; Custom ELPA packages.
(setq starter-kit-packages
      (list
       'clojure-mode
       'clojure-test-mode
       'closure-template-html-mode
       'color-theme
       'css-mode
       'find-file-in-project
       'gist
       'haml-mode
       'inf-ruby
       'js2-mode
       'json
       'ruby-mode
       'ruby-test-mode
       'sass-mode
       'slime-repl
       'smart-tab
       'yaml-mode
       'yasnippet-bundle))

;; Install the custom ELPA packages, if not already installed.
(starter-kit-elpa-install)

;; Delete trailing whitespace when saving.
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

;; Show the menu-bar.
(if (fboundp 'menu-bar-mode) (menu-bar-mode t))

;; Use custom color theme.
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

;; AUTO-COMPLETE - SLIME
(require 'slime)
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)

;; LEININGEN
(defun lein-deps ()
  "Run lein deps and update all dependencies of the current
project."
  (interactive)
  (let ((root (locate-dominating-file default-directory "project.clj")))
    (when (not root)
      (error "Hey Cowboy, yOu are nOt in any Leiningen prOject!"))
    ;; you can customize slime-port using .dir-locals.el
    (shell-command (format "cd %s && lein deps &" root) "*lein-deps*")))

(defun lein-ring-server ()
  "Run 'lein ring server' in the current project to start a web
server."
  (interactive)
  (let ((root (locate-dominating-file default-directory "project.clj")))
    (when (not root)
      (error "Hey Cowboy, yOu are nOt in any Leiningen prOject!"))
    (shell-command (format "cd %s && lein ring server &" root)) "*lein-ring-server*"))

;; SLIME

;; Fix problem with unicode characters in source code.
;; http://stackoverflow.com/questions/3101279/how-do-i-use-unicode-utf-8-characters-in-clojure-regular-expressions
(custom-set-variables '(slime-net-coding-system (quote utf-8-unix)))

;; COFFEE MODE
(let ((coffee-mode-directory "~/.emacs.d/coffee-mode"))
  (when (file-directory-p coffee-mode-directory)
    (add-to-list 'load-path coffee-mode-directory)
    (require 'coffee-mode)
    (defun coffee-mode-customization ()
      (setq coffee-tab-width 2
            tab-width 2)
      (define-key coffee-mode-map "\C-c\C-k" 'coffee-compile-buffer)
      (define-key coffee-mode-map "\C-c\C-r" 'coffee-compile-region))
    (add-hook 'coffee-mode-hook '(lambda () (coffee-mode-customization)))))

;;; COMPILE-MODE
(setq compilation-scroll-output 't)

;; CLOJURE-MODE
(defun define-clojure-indent-words ()
  (define-clojure-indent (DELETE 1))
  (define-clojure-indent (GET 1))
  (define-clojure-indent (POST 1))
  (define-clojure-indent (PUT 1))
  (define-clojure-indent (admin-db-test 1))
  (define-clojure-indent (analytic-db-test 1))
  (define-clojure-indent (api-admin-test 1))
  (define-clojure-indent (api-test 1))
  (define-clojure-indent (are 1))
  (define-clojure-indent (context 1))
  (define-clojure-indent (controller-test 1))
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

;; CLOJURE SCRIPT
(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

;; CLOSURE-LINT-MODE
(require 'closure-lint-mode)

;; CLOSURE-TEMPLATE-HTML-MODE
(require 'closure-template-html-mode)

;; DURENDAL
(require 'durendal)

;; FIND-FILE-IN-PROJECT
(setq ffip-patterns '("*.coffee" "*.rb" "*.html" "*.el" "*.js" "*.rhtml"))

;; GIST
(setq gist-view-gist t)

;; JAVA
(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook 'java-mode-indent-annotations-setup)

;; RVM
(require 'rvm)
(set 'rvm-executable (if (file-exists-p "~/.rvm/bin/rvm") "~/.rvm/bin/rvm" "/usr/local/bin/rvm"))
(rvm-use-default)
(setenv "rvm_path" "/usr/local/rvm")

;; ESHELL
(require 'eshell-ext)

;; FLYSPELL MODE.
(dolist (hook '(LaTeX-mode-hook)) (add-hook hook 'flyspell-mode))
(setq flyspell-abbrev-p t)

;; FLYMAKE (fix the annoying dialog)
(require 'flymake)
(defun flymake-report-fatal-status (status warning)
  "Display a warning and switch flymake mode off."
  (message (format "Flymake: %s. Flymake will be switched OFF" warning))
  (flymake-mode 0))

;; MAGIT
(require 'magit)

;; GIT-BLAME-MODE
(dolist (filename '("/usr/share/emacs/site-lisp/git-blame.el"
                    "/usr/share/git/emacs/git-blame.el"))
  (if (file-exists-p filename) (load-file filename)))

;; HASKELL-MODE
(require 'haskell-mode)
(require 'inf-haskell)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))

;; HIPPIE EXPAND
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name))

;; PAREDIT
(require 'paredit)
(add-hook 'eshell-mode-hook 'disable-paredit-mode)

;; RCIRC
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

;; SASS
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

;; SMART-TAB
(setq smart-tab-using-hippie-expand t)
(dolist
    (hook
     '(
       LaTeX-mode-hook
       c-mode-hook
       css-mode-hook
       coffee-mode-hook
       emacs-lisp-mode-hook
       haml-mode-hook
       html-mode-hook
       java-mode-hook
       paredit-mode
       ruby-mode-hook
       slime-mode-hook
       yaml-mode-hook))
  (add-hook hook (lambda () (smart-tab-mode t))))

;; SOY-MODE
(require 'soy-mode)

;; TRAMP
(require 'tramp)
(tramp-set-completion-function
 "ssh"
 '((tramp-parse-shosts "~/.ssh/known_hosts")
   (tramp-parse-hosts "/etc/hosts")))

;; COMINT MODE
(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 )

;; RE-BUILDER (PERL)
(require 're-builder-x)

;; RUBY-TEST MODE
(require 'ruby-test-mode)
(setq ruby-test-ruby-executables '("/usr/local/rvm/rubies/ruby-1.9.2-p136/bin/ruby")
      ruby-test-rspec-executables '("/usr/local/rvm/gems/ruby-1.9.2-p136/bin/rspec"))

;; SQL-MODE
(let ((filename "~/.sql.el")) (if (file-exists-p filename) (load-file filename)))

;; EMACS RAILS RELOADED
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

(defadvice paredit-open-round (after paredit-open-round-js-advice) ()
  "Delete the whitespace before when using Paredit in Javascript modes."
  (backward-char)
  (delete-backward-char 1)
  (forward-char))

;; ;; EMMS
;; (require 'emms-setup)
;; (require 'emms-player-mplayer)
;; (emms-standard)
;; (emms-default-players)
;; (setq emms-source-file-default-directory "~/Music")

;; YASNIPPET
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/completing-prompt))

;; KEY BINDINGS

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
(global-set-key [f5] 'compile)

;; Jump into eshell
(eshell)
