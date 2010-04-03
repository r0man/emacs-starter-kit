
(setq mail-from-style nil
      message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-extra-arguments '("-a" "Soundcloud")
      sendmail-program "/usr/bin/msmtp"
      user-full-name "Roman Scherer"
      user-mail-address "roman@soundcloud.com")

;;; Default method for selecting a newsgroup.
(setq gnus-select-method '(nnfolder ""))

;;; A list of secondary methods that will be used for reading news.
(setq gnus-secondary-select-methods
      '((nnimap "Burningswell"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnimap-expunge-on-close always)
                (nnimap-authinfo-file "~/.authinfo"))
        (nnimap "Soundcloud"
                (nnimap-address "imap.gmail.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnimap-expunge-on-close always)
                (nnimap-authinfo-file "~/.authinfo"))))

;;; Minor mode for topicsifying Gnus group buffers.
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;;; Alist of styles to use when posting.
(setq gnus-posting-styles
      '(("Burningswell"
         (address "roman.scherer@burningswell.com")
         (eval (setq message-sendmail-extra-arguments '("-a" "Burningswell"))))
        ("Soundcloud"
         (address "roman@soundcloud.com")
         (eval (setq message-sendmail-extra-arguments '("-a" "Soundcloud")))
         (signature-file "~/.emacs.d/soundcloud.signature"))))

;;; List of functions used for sorting threads in the summary
;;; buffer. By default, threads are sorted by article number.
(setq gnus-thread-sort-functions
      ' gnus-thread-sort-by-most-recent-date)

(gnus-demon-add-handler 'gnus-group-get-new-news 2 t)

