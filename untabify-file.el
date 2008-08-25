(require 'cl)
(require 'custom)

(defcustom untabify-exclude-list
  '(makefile-mode
    makefile-bsdmake-mode
    change-log-mode
    "Makefile$")
  "List of regexp or modes to which is not applied untabify."
  :group 'untabify)

(defun untabify-before-write ()
  "Strip all trailing whitespaces and untabify buffer before
save."
  (when (and (eq this-command 'save-buffer)
             (not (find nil
                        untabify-exclude-list
                        :if #'(lambda (r)
                                (typecase r
                                  (string (string-match r (buffer-name)))
                                  (symbol (eq major-mode r)))))))
    (save-excursion
      (untabify (point-min) (point-max))
      (delete-trailing-whitespace) ;; TtT: causes issues when sharing code ;; 

)))

(add-hook 'write-file-hooks 'untabify-before-write)

(provide 'untabify-file)
