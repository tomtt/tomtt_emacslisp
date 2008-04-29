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

(provide 'my-snippets)
