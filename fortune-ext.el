(require 'fortune)

(defun fortune-epigram (&optional file-or-dir)
  "Returns a random, hopefully interesting, adage from
`fortune'. If the optional FILE-OR-DIR argument is given, the
fortune will be take from this file or directory, otherwise from
`fortune-file'."
  (save-excursion
    (fortune-in-buffer (or file-or-dir fortune-file))
    (let ((buffer (get-buffer fortune-buffer-name)))
      (set-buffer buffer)
      (chomp
       (replace-regexp-in-string
        "\n" " "
        (buffer-substring (point-min) (point-max)))))))

(defun fortune-chuck-norris-epigram ()
  "Returns a random Chuck Norris joke from fortune."
  (fortune-epigram "~/.emacs.d/fortune-chucknorris"))

(provide 'fortune-ext)
