
(setq smtp-accounts
      '((ssl "roman.scherer@burningswell.com" "smtp.gmail.com" 587 nil nil)
        (ssl "roman@soundcloud.com" "smtp.gmail.com" 587 nil nil)))

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

(setq gnus-posting-styles
      '(("Burningswell" (address "roman.scherer@burningswell.com"))
        ("Soundcloud" (address "roman@soundcloud.com"))))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(require 'smtpmail)

(setq mail-from-style nil
      message-send-mail-function 'smtpmail-send-it
      send-mail-function 'smtpmail-send-it
      smtpmail-debug-info t
      smtpmail-debug-verb t
      user-full-name "Roman Scherer"
      user-mail-address "roman@soundcloud.com")

(defun set-smtp-plain (address server port)
  "Set related SMTP variables for supplied parameters."
  (setq smtpmail-auth-credentials "~/.authinfo"
        smtpmail-smtp-service port
        smtpmail-starttls-credentials nil
        smtpmail-smtp-server server
        user-mail-address address)
  (message "Setting SMTP server for '%s' to '%s:%s'." address server port))

(defun set-smtp-ssl (address server port key cert)
  "Set related SMTP and SSL variables for supplied parameters."
  (setq smtpmail-auth-credentials "~/.authinfo"
        smtpmail-smtp-server server
        smtpmail-smtp-service port
        smtpmail-starttls-credentials (list (list server port key cert))
        starttls-extra-arguments nil
        starttls-gnutls-program "gnutls-cli"
        starttls-use-gnutls t
        user-mail-address address)
  (message "Setting SMTP server for '%s' to '%s:%s' (SSL)." address server port))

(defun change-smtp ()
  "Change the SMTP server according to the current from line."
  (save-excursion
    (loop with from = (save-restriction
                        (message-narrow-to-headers)
                        (message-fetch-field "from"))
          for (acc-type address server port key cert) in smtp-accounts
          when (string-match address from)
          do (cond
              ((eql acc-type 'plain)
               (return (set-smtp-plain address server port key cert)))
              ((eql acc-type 'ssl)
               (return (set-smtp-ssl address server port key cert)))
              (t (error "Unrecognized SMTP account type: '%s'." acc-type)))
          finally (error "Cannot interfere SMTP information."))))

(add-hook 'message-send-hook 'change-smtp)
