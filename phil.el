;;; Fonts

(defun inconsolata (size)
  (interactive "p")
  (set-default-font
   (concat "-unknown-Inconsolata-normal-normal-normal-*-"
           (if (stringp size) size
             (if (= 1 size) "16"
               (read-from-minibuffer "Size: ")))
           "-*-*-*-m-0-*-*")))

(defun envy (size)
  (interactive "p")
  (set-default-font
   (concat "-unknown-Envy Code R-normal-normal-normal-*-"
           (if (stringp size) size
             (if (= 1 size) "16"
               (read-from-minibuffer "Size: ")))
           "-*-*-*-m-0-iso10646-1")))

(defun dejavu (size)
  (interactive "p")
  (set-default-font
   (concat "-dejavu-dejavu sans mono-medium-r-normal--"
           (if (stringp size) size
             (if (= 1 size) "16"
               (read-from-minibuffer "Size: ")))
           "-*-0-0--iso8859-1")))

;;; IRC

(eval-after-load 'rcirc
  '(progn
     (require 'rcirc-color)
     (require 'rcirc-completion)
     (require 'dbus)

     (dbus-register-signal :system "org.freedesktop.NetworkManager"
                      "/org/freedesktop/NetworkManager" "org.freedesktop.NetworkManager"
                      "StateChanged"
                      'handle-network-state-change)

     (setq rcirc-authinfo '(("freenode" nickserv "technomancy" "technomancy"))
           rcirc-default-nick "technomancy"
           rcirc-server-alist '(("irc.freenode.net"
                                 :channels ("#emacs" "#seattle.rb"
                                            "#clojure" "#sonian" "#sonian-safe"))))

     (add-hook 'rcirc-mode-hook (lambda ()
                                  (set (make-local-variable 'scroll-conservatively)
                                       8192)
                                  (rcirc-track-minor-mode 1)
                                  (rcirc-omit-mode)
                                  (flyspell-mode 1)))))

(defun handle-network-state-change (&rest state)
  ;; this doesn't seem to work
  (when (member 4 state)
    (message "Disconnected!")
    (rcirc-cmd-quit "disconnected"))
  ;; 3 is magic-dbus-speak for "CONNECTED"
  (when (member 3 state)
    (rcirc nil)))

(defun irc ()
  (interactive)
  (inconsolata 1)
  (rcirc nil)
  (split-window-horizontally))

;;; elisp libraries I run from source checkouts:

(add-to-list 'load-path "/home/phil/src/emacs-w3m")
(add-to-list 'load-path "/home/phil/src/relax.el")
(add-to-list 'load-path "/home/phil/src/elim/elisp")
(add-to-list 'load-path "/home/phil/src/clojure-mode")

(autoload 'w3m "w3m" "w3m browser" t)
(autoload 'relax "relax" "Connect to the CouchDB database at db-url." t)
(autoload 'garak "garak" "Start Garak IM session." t)

;;; Random stuff

;; If we don't have XFT, let's at least pick a decent default.
(if (< emacs-major-version 23)
    (ignore-errors
      (set-default-font "-xos4-terminus-medium-r-normal--16-160-72-72-c-80-iso8859-1")))

(eval-after-load 'ruby-mode
  '(add-hook 'ruby-mode-hook 'esk-paredit-nonlisp))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/home/phil/src/conkeror/contrib/run-conkeror")

(clojure-slime-config)

;; (eval-after-load 'swank-clojure
;;   '(add-to-list 'swank-clojure-extra-vm-args
;;                 "-agentlib:jdwp=transport=dt_socket,address=8021,server=y,suspend=n"))

(add-hook 'slime-repl-mode-hook 'turn-on-paredit)

(add-hook 'clojure-mode-hook 'turn-on-whitespace)

;; unfortunately some codebases use tabs. =(
(set-default 'tab-width 4)
(set-default 'c-basic-offset 2)

;; javadoc
(eval-after-load 'cc-mode
  '(progn
     (define-key java-mode-map (kbd "C-M-h") 'backward-kill-word)))

;; Get around the emacswiki spam protection
(add-hook 'oddmuse-mode-hook
          (lambda ()
            (unless (string-match "question" oddmuse-post)
              (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post)))))

(defun eshell/rm (&rest args)
  "Eshell's built-in rm is ridiculously slow."
  (shell-command (format "rm %s" (mapconcat 'identity args " "))))

(defalias 'zb 'color-theme-zenburn)
(defalias 'bb 'color-theme-blackboard)

;;; Paredit hacks
(load "../../paredit/paredit-beta")
(load "../../paredit/paredit-delimiter-space")
(load "../../paredit/paredit-semicolon")

;;; broken ido
(defun ido-directory-too-big-p (arg) nil)

;;; registers
(set-register ?n '(file . "~/documents/notes.org"))
(set-register ?p '(file . "~/.emacs.d/phil.el"))

;; processing
(add-to-list 'load-path "/home/phil/src/processing-emacs/")
(autoload 'processing-mode "processing-mode" "Processing mode" t)
(add-to-list 'auto-mode-alist '("\\.pde$" . processing-mode))
(setq processing-location "/home/phil/src/processing-1.0.6/")

;; Java crap

(defun insert-property ()
  "XML! it's awesome."
  (interactive)
  (let ((name (read-from-minibuffer "Property Name: "))
        (value (read-from-minibuffer "Property Value: ")))
    (insert (format
             "<property>\n  <name>%s</name>\n  <value>%s</value>\n</property>\n"
             name value))))
