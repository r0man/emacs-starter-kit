(require 'erc-extension)
(require 'erc-highlight-nicknames)
(require 'erc-join)
(require 'erc-nick-notify)
(require 'erc-robot)
(require 'erc-services)
(require 'fortune-ext)

;; Join channels in automatical.
(erc-autojoin-mode 1)

;; Enable ERC highlight-nicknames mode.
(erc-highlight-nicknames-enable)

;; Enable flyspell mode in ERC buffers.
(erc-spelling-mode 1)

;; Use the user-full-name variable on IRC.
(setq erc-user-full-name user-full-name)

;; This mode automates communication with services.
(erc-services-mode 1)

;; Don't ask for the password when identifying to NickServ.
(setq erc-prompt-for-nickserv-password nil)

(defun erc-cmd-SLAP (&rest nick)
  "Slaps someone around the solar system -- just out of spite."
  (erc-send-action (erc-default-target) (concat "slaps " (car nick) " around the solar system -- just out of spite!")))

(defun erc-cmd-SPOOK (&rest ignore)
  "Send a spooky list of keywords."
  (let* ((spook (with-temp-buffer (spook) (buffer-string)))
	 (output (replace-regexp-in-string "\n" " " spook)))
    (erc-send-message output)))

(defun erc-cmd-YOW ()
  "Display some pinhead wisdom into the current ERC buffer."
  (let ((yow (replace-regexp-in-string "\n" "" (yow))))
    (erc-send-message yow)))

(setq erc-robot-commands
      '(
        ("chucknorris" t (lambda (args) (fortune-chuck-norris-epigram)))
        ("echo" t (lambda (args) args))
        ("help" t (lambda (args)
                    (concat
                     "Commands: "
                     (mapconcat
                      (lambda (e)
                        (car e))
                      erc-robot-commands " "))))
        ("version" t (lambda (args) (erc-version)))
        ("zippy" t (lambda (args)
                     (erc-replace-regexp-in-string "\n" " " (yow))))))

(add-hook 'erc-server-PRIVMSG-functions 'erc-robot-remote t)
(add-hook 'erc-send-completed-hook 'erc-robot-local t)

(provide 'erc-customize)
