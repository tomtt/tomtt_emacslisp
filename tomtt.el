;; my personal preferences
(partial-completion-mode t)

(snippet-with-abbrev-table 'text-mode-abbrev-table
  ("ehtml" .  "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Strict//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">
<html lang=\"en\">
  <head>
    <meta http-equiv=\"Content-Type\" content=\"text/html;charset=UTF-8\" />
    <meta name=\"keywords\" content=\"\" />
    <title>$${title}</title>
    <link rel=\"stylesheet\" type=\"text/css\" href=\"/stylesheets/common.css\" />$${
    <script src=\"/javascripts/common.js\" type=\"text/javascript\"></script>}
  </head>
  <body$${ onLoad=\"show_javascript_enabled();\"}>$${
    <div class=\"container\">
      $.
    </div>}
  </body>
</html>
"))

(snippet-with-abbrev-table 'ruby-mode-abbrev-table
  ("def" . "def $${funtion_name} $${args}
  $.
end"))

(snippet-with-abbrev-table 'ruby-mode-abbrev-table
  ("pv" . "$${var}=#{$${var}} "))

(defun html-indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if snippet
      (snippet-next-field)
    (if (looking-at "\\>")
	(progn
	  (hippie-expand nil)
	  (indent-for-tab-command))
      (indent-for-tab-command))))

(require 'sgml-mode)
(define-key emacs-lisp-mode-map "\C-j" 'eval-last-sexp)
(define-key html-mode-map "\t" 'html-indent-or-complete)

(require 'gnuserv)
(gnuserv-start)
(setq gnuserv-frame (selected-frame))
(setenv "GNUSERV_SHOW_EMACS" "1")

(provide 'tomtt)

(shell-command "cygpath -u tomtt")

(snippet-with-abbrev-table 'ruby-mode-abbrev-table
  ("prs" . "    person_id: $${id}"))
