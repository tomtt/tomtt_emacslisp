;; my personal preferences
;;(partial-completion-mode t)

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

(snippet-with-abbrev-table 'text-mode-abbrev-table
  ("sld" .  "<div class=\"slide\">
  <h1>$${title}</h1>
  <p class=\"subhead\">$${subhead}</p>
  $.
</div>"))


(snippet-with-abbrev-table 'ruby-mode-abbrev-table
  ("def" . "def $${funtion_name} $${args}
  $.
end"))

(snippet-with-abbrev-table 'ruby-mode-abbrev-table
  ("pv" . "$${var}=#{$${var}} "))

(snippet-with-abbrev-table 'ruby-mode-abbrev-table
  ("it" . "$>it \"$${should}\" do
$>$.
  end"))

(snippet-with-abbrev-table 'ruby-mode-abbrev-table
  ("iteo" . "$>it \"should have an error on $${attribute}\" do
$>@$${obj}.attributes = valid_$${obj}_attributes.except(:$${attribute})
$>@$${obj}.should have(1).error_on(:$${attribute})
  end"))

(snippet-with-abbrev-table 'ruby-mode-abbrev-table
  ("shelp" . "lib_path = File.expand_path(\"#{File.dirname(__FILE__)}/../lib\")
$LOAD_PATH.unshift lib_path unless $LOAD_PATH.include?(lib_path)

require 'rubygems'
require 'spec'
require 'ruby-debug'
"))

(snippet-with-abbrev-table 'ruby-mode-abbrev-table
  ("spec" . "require File.dirname(__FILE__) + '/spec_helper'
require '$${class}'

describe $${Class} do
$>before(:each) do
$>@$${class} = $${Class}.new
  end

$>$.
end"))

(snippet-with-abbrev-table 'ruby-mode-abbrev-table
  ("matcher" . "class $${MatchThing}
  def initialize($${expected})
    @$${expected} = $${expected}
  end

  def matches?(actual)
    @actual = actual
    # Satisfy expectation here. Return false or raise an error if it's not met.
    $.
  end

  def failure_message
    \"expected #{@actual.inspect} to $${match_thing} #{@$${expected}.inspect}, but it didn't\"
  end

  def negative_failure_message
    \"expected #{@actual.inspect} not to $${match_thing} #{@$${expected}.inspect}, but it did\"
  end
end

def $${match_thing}($${expected})
  $${MatchThing}.new($${expected})
end"))

(snippet-with-abbrev-table 'ruby-mode-abbrev-table
  ("nsev" . "<%= show_event_detail('$${name}', Date.new(200$${8}, $${m}, $${d}), '$${time}', '$${location}', '$${price}', '$${band}', '$${formal}', '$${info}') %>"))
(snippet-with-abbrev-table 'ruby-mode-abbrev-table
  ("nspgm" . "<%= render(:partial => \"$${label}_programme\") %>\n$><a href=\"/page/events/$${label}_programme\">View programme on single page</a>"))
(snippet-with-abbrev-table 'ruby-mode-abbrev-table
  ("nspgmdef" . "def $${dance}_2008
    programme_text = <<EOT
$.
EOT
  create_program('$${dance}_2008', programme_text, \"$${name}\")
end"))

(snippet-with-abbrev-table 'haml-mode-abbrev-table
  ("li" . "    %li
$>$."))

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

;(require 'gnuserv)
;(gnuserv-start)
;(setq gnuserv-frame (selected-frame))
;(setenv "GNUSERV_SHOW_EMACS" "1")

(fset 'insert-selection-as-color
   [?\C-y ?\C-  M-left left ?\C-x ?r ?s ?c ?\C-w M-left M-right ?\C-k ?\C-  M-left ?\C-x ?r ?s ?n ?\C-w ?\C-x ?r ?i ?c M-right ?\; ?  ?\C-c ?\C-c ?\C-x ?r ?i ?n ?\C-e left return])
(define-key cssm-mode-map "\C-c \C-i" 'insert-selection-as-color)
(define-key cssm-mode-map "\C-c \C-l" 'list-colors-display)

(defun tomtt-planner-config ()
  (define-key planner-mode-map "\C-c\C-q" 'planner-task-open)
  (define-key planner-mode-map "\C-c\C-w" 'planner-task-pending))
(add-hook 'planner-mode-hook 'tomtt-planner-config)

(defun open-file-as-log (&optional log-file)
  (interactive)
  (let* ((log-file (if log-file log-file "c:/tomtt/cygwin/tmp/bla"))
         (buffer (rails-log:buffer-name log-file))
         (current (buffer-name)))
    (unless (get-buffer buffer)
      (get-buffer-create buffer)
      (set-buffer buffer)
      (setq auto-window-vscroll t)
      (setq buffer-read-only t)
      (set-buffer current)
      (apply-colorize-to-buffer buffer))
    (start-process "tail"
                   buffer
                   "tail"
                   "-n 130"
                   "-f" log-file)))

(define-keys rails-minor-mode-map
  ((rails-key "\C-c i s") 'it-start)
  ((rails-key "\C-c i p") 'it-pend)
  ((rails-key "\C-c i u") 'it-unpend))


(provide 'tomtt)

