
(setq mail-from-style nil
      message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-extra-arguments '("-a" "Soundcloud")
      sendmail-program "/usr/bin/msmtp"
      user-full-name "Roman Scherer"
      user-mail-address "roman@soundcloud.com")

(setq gnus-select-method '(nnfolder ""))

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

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-posting-styles
      '(("Burningswell"
         (address "roman.scherer@burningswell.com")
         (eval (setq message-sendmail-extra-arguments '("-a" "Burningswell"))))
        ("Soundcloud"
         (address "roman@soundcloud.com")
         (eval (setq message-sendmail-extra-arguments '("-a" "Soundcloud")))
         (signature-file "~/.emacs.d/soundcloud.signature"))))

(gnus-demon-add-handler 'gnus-group-get-new-news 2 t)
(gnus-demon-init)

