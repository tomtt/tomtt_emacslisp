;;; org-export-latex.el --- LaTeX exporter for org-mode
;;
;; Copyright (c) 2007 free software foundation, inc.
;;
;; Emacs Lisp Archive Entry
;; Filename: org-export-latex.el
;; Version: 5.12
;; Author: Bastien Guerry <bzg AT altern DOT org>
;; Maintainer: Bastien Guerry <bzg AT altern DOT org>
;; Keywords: org, wp, tex
;; Description: Converts an org-mode buffer into LaTeX
;; URL: http://www.cognition.ens.fr/~guerry/u/org-export-latex.el
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
;; more details.
;; 
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs; see the file COPYING. If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.
;;
;;; Commentary:
;;
;; This library implements a LaTeX exporter for org-mode.
;; 
;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'org-export-latex)
;; 
;; The interactive functions are similar to those of the HTML exporter:
;; 
;; M-x `org-export-as-latex'
;; M-x `org-export-as-latex-batch'
;; M-x `org-export-as-latex-to-buffer'
;; M-x `org-export-region-as-latex'
;; M-x `org-replace-region-by-latex'
;;
;;; Code:

(eval-when-compile
  (require 'cl))

(require 'footnote)
(require 'org)

;;; Variables:
(defvar org-latex-options-plist nil)
(defvar org-latex-todo-keywords-1 nil)
(defvar org-latex-all-targets-regexp nil)
(defvar org-latex-add-level 0)
(defvar org-latex-sectioning-depth 0)
(defvar org-export-latex-list-beginning-re
  "^\\([ \t]*\\)\\([-+*]\\|[0-9]+[.)]\\) +?")

(defvar org-latex-special-string-regexps
  '(org-ts-regexp
    org-scheduled-string
    org-deadline-string
    org-clock-string)
  "A list of regexps to convert as special keywords.")

(defvar latexp)    ; dynamically scoped from org.el
(defvar re-quote)  ; dynamically scoped from org.el
(defvar commentsp) ; dynamically scoped from org.el

;;; Custom variables:
(defcustom org-export-latex-sectioning-alist
  '((1 "\\section{%s}" "\\section*{%s}")
    (2 "\\subsection{%s}" "\\subsection*{%s}")
    (3 "\\subsubsection{%s}" "\\subsubsection*{%s}")
    (4 "\\paragraph{%s}" "\\paragraph*{%s}")
    (5 "\\subparagraph{%s}" "\\subparagraph*{%s}"))
  "Alist of LaTeX commands for inserting sections.
Here is the structure of each cell:

  \(level unnumbered-section numbered-section\)

The %s formatter will be replaced by the title of the section."
  :group 'org-export-latex
  :type 'alist)

(defcustom org-export-latex-emphasis-alist
  '(("*" "\\textbf{%s}" nil)
    ("/" "\\emph{%s}" nil)
    ("_" "\\underline{%s}" nil)
    ("+" "\\texttt{%s}" nil)
    ("=" "\\texttt{%s}" nil))
  "Alist of LaTeX expressions to convert emphasis fontifiers.
Each element of the list is a list of three elements.
The first element is the character used as a marker for fontification.
The second element is a formatting string to wrap fontified text with.
The third element decides whether to protect converted text from other
conversions."
  :group 'org-export-latex
  :type 'alist)

(defcustom org-export-latex-preamble
  "\\documentclass[11pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{hyperref}"
  "Preamble to be inserted at the very beginning of the LaTeX export."
  :group 'org-export-latex
  :type 'string)

(defcustom org-export-latex-title-command "\\maketitle"
  "The command used to insert the title just after \\begin{document}.
If this string contains the formatting specification \"%s\" then
it will be used as a formatting string, passing the title as an
argument."
  :group 'org-export-latex
  :type 'string)

(defcustom org-export-latex-date-format 
  "%d %B %Y"
  "Format string for \\date{...}."
  :group 'org-export-latex
  :type 'string)

(defcustom org-export-latex-tables-verbatim nil
  "When non-nil, export tables as verbatim."
  :group 'org-export-latex
  :type 'boolean)

(defcustom org-export-latex-packages-alist nil
  "Alist of packages to be inserted in the preamble.
Each cell is of the forma \( option . package \).

For example:

\(setq org-export-latex-packages-alist
      '((\"french\" \"babel\"))"
  :group 'org-export-latex
  :type 'alist)

(defcustom org-export-latex-low-levels 'description
  "How to convert sections below the current level of sectioning,
as specified by `org-export-headline-levels' or the value of \"H:\"
in Org's #+OPTION line.

This can be either nil (skip the sections), 'description (convert
the sections as descriptive lists) or a string to be used instead
of \\section{%s}. In this latter case, the %s stands here for the
inserted headline and is mandatory."
  :group 'org-export-latex
  :type '(choice (const :tag "Ignore" nil)
		 (symbol :tag "Convert as descriptive list" description)
		 (string :tag "Use a section string" :value "\\subparagraph{%s}")))

(defcustom org-export-latex-remove-from-headlines
  '(:todo t :priority t :tags t)
  "A plist of keywords to remove from headlines.
Non-nil means remove this keyword type from the headline.

Don't remove the keys, just change their values."
  :type 'plist
  :group 'org-export-latex)

(defcustom org-export-latex-image-default-option "width=10em"
  "Default option for images."
  :group 'org-export-latex
  :type '(string))

(defcustom org-export-latex-coding-system nil
  "Coding system for the exported LaTex file."
  :group 'org-export-latex
  :type 'coding-system)

;; FIXME Do we want this one?
;; (defun org-export-as-latex-and-open (arg) ...)

;;; Autoload functions:
;;;###autoload
(defun org-export-as-latex-batch ()
  "Call `org-export-as-latex', may be used in batch processing as
emacs 	--batch
	--load=$HOME/lib/emacs/org.el
	--eval \"(setq org-export-headline-levels 2)\"
	--visit=MyFile --funcall org-export-as-latex-batch"
  (org-export-as-latex org-export-headline-levels 'hidden))

;;;###autoload
(defun org-export-as-latex-to-buffer (arg)
  "Call `org-exort-as-latex` with output to a temporary buffer.
No file is created.  The prefix ARG is passed through to `org-export-as-latex'."
  (interactive "P")
  (org-export-as-latex arg nil nil "*Org LaTeX Export*")
  (switch-to-buffer-other-window "*Org LaTeX Export*"))

;;;###autoload
(defun org-replace-region-by-latex (beg end)
  "Replace the region from BEG to END with its LaTeX export.
It assumes the region has `org-mode' syntax, and then convert it to
LaTeX. This can be used in any buffer. For example, you could
write an itemized list in `org-mode' syntax in an LaTeX buffer and
then use this command to convert it."
  (interactive "r")
  (let (reg latex buf)
    (save-window-excursion
      (if (org-mode-p)
	  (setq latex (org-export-region-as-latex
		       beg end t 'string))
	(setq reg (buffer-substring beg end)
	      buf (get-buffer-create "*Org tmp*"))
	(save-excursion
	  (set-buffer buf)
	  (erase-buffer)
	  (insert reg)
	  (org-mode)
	  (setq latex (org-export-region-as-latex
		       (point-min) (point-max) t 'string)))
	(kill-buffer buf)))
    (delete-region beg end)
    (insert latex)))

;;;###autoload
(defun org-export-region-as-latex (beg end &optional body-only buffer)
  "Convert region from BEG to END in `org-mode' buffer to LaTeX.
If prefix arg BODY-ONLY is set, omit file header, footer, and table of
contents, and only produce the region of converted text, useful for
cut-and-paste operations.
If BUFFER is a buffer or a string, use/create that buffer as a target
of the converted LaTeX.  If BUFFER is the symbol `string', return the
produced LaTeX as a string and leave not buffer behind.  For example,
a Lisp program could call this function in the following way:

  (setq latex (org-export-region-as-latex beg end t 'string))

When called interactively, the output buffer is selected, and shown
in a window.  A non-interactive call will only retunr the buffer."
  (interactive "r\nP")
  (when (interactive-p)
    (setq buffer "*Org LaTeX Export*"))
  (let ((transient-mark-mode t) (zmacs-regions t)
	rtn)
    (goto-char end)
    (set-mark (point)) ;; to activate the region
    (goto-char beg)
    (setq rtn (org-export-as-latex
	       nil nil nil
	       buffer body-only))
    (if (fboundp 'deactivate-mark) (deactivate-mark))
    (if (and (interactive-p) (bufferp rtn))
	(switch-to-buffer-other-window rtn)
      rtn)))

;;;###autoload
(defun org-export-as-latex (arg &optional hidden ext-plist
				to-buffer body-only)
  "Export current buffer to a LaTeX file."
  (interactive "P")
  ;; Make sure we have a file name when we need it.
  (when (and (not (or to-buffer body-only))
	     (not buffer-file-name))
    (if (buffer-base-buffer)
	(org-set-local 'buffer-file-name
		       (with-current-buffer (buffer-base-buffer)
			 buffer-file-name))
      (error "Need a file name to be able to export")))

  (message "Exporting to LaTeX...")
  (org-update-radio-target-regexp)
  (org-export-latex-set-initial-vars ext-plist)
  (let* ((wcf (current-window-configuration))
	 (opt-plist org-latex-options-plist)
	 (filename (concat (file-name-as-directory
			    (org-export-directory :LaTeX ext-plist))
			   (file-name-sans-extension
			    (file-name-nondirectory ;sans-extension
			     buffer-file-name)) ".tex"))
	 (filename (if (equal (file-truename filename)
			      (file-truename buffer-file-name))
		       (concat filename ".tex")
		     filename))
	 (buffer (if to-buffer
		     (cond
		      ((eq to-buffer 'string) (get-buffer-create
					       "*Org LaTeX Export*"))
		      (t (get-buffer-create to-buffer)))
		   (find-file-noselect filename)))
	 (region-p (org-region-active-p))
	 (odd org-odd-levels-only)
	 (preamble (org-export-latex-make-preamble opt-plist))
	 (skip (plist-get opt-plist :skip-before-1st-heading))
	 (text (plist-get opt-plist :text))
	 (first-lines (if skip "" (org-export-latex-first-lines)))
	 (coding-system (and (boundp 'buffer-file-coding-system)
			     buffer-file-coding-system))
	 (coding-system-for-write (or org-export-latex-coding-system
				      coding-system))
	 (save-buffer-coding-system (or org-export-latex-coding-system
					coding-system))
         (region (buffer-substring
		  (if region-p (region-beginning) (point-min))
		  (if region-p (region-end) (point-max))))
	 (string-for-export
	  (org-cleaned-string-for-export
	   region :emph-multiline t
		  :for-LaTeX t
		  :comments nil
		  :add-text (if (eq to-buffer 'string) nil text)
		  :skip-before-1st-heading skip
		  :LaTeX-fragments nil)))

    (set-buffer buffer) 
    (erase-buffer)

    (and (fboundp 'set-buffer-file-coding-system)
	 (set-buffer-file-coding-system coding-system-for-write))

    ;; insert the preamble and initial document commands
    (unless (or (eq to-buffer 'string) body-only)
      (insert preamble))

    ;; insert text found in #+TEXT
    (when (and text (not (eq to-buffer 'string)))
      (insert (org-export-latex-content text) "\n\n"))

    ;; insert lines before the first headline
    (unless (or skip (eq to-buffer 'string))
      (insert first-lines))

    ;; handle the case where the region does not begin with a section
    (when region-p
      (insert (with-temp-buffer
		(insert string-for-export)
		(org-export-latex-first-lines))))

    ;; export the content of headlines
    (org-export-latex-global
     (with-temp-buffer
       (insert string-for-export)
       (goto-char (point-min))
       (when (re-search-forward "^\\(\\*+\\) " nil t)
	 (let* ((asters (length (match-string 1)))
		(level (if odd (- asters 2) (- asters 1))))
	   (setq org-latex-add-level
		 (if odd (1- (/ (1+ asters) 2)) (1- asters)))
	   (org-export-latex-parse-global level odd)))))

    ;; finalization
    (unless body-only (insert "\n\\end{document}"))
    (or to-buffer (save-buffer))
    (goto-char (point-min))
    (message "Exporting to LaTeX...done")
    (prog1
	(if (eq to-buffer 'string)
	    (prog1 (buffer-substring (point-min) (point-max))
	      (kill-buffer (current-buffer)))
	  (current-buffer))
      (set-window-configuration wcf))))


;;; Parsing functions:
(defun org-export-latex-parse-global (level odd)
  "Parse the current buffer recursively, starting at LEVEL.
If ODD is non-nil, assume the buffer only contains odd sections.
Return A list reflecting the document structure."
  (save-excursion
    (goto-char (point-min))
    (let* ((cnt 0) output
	   (depth org-latex-sectioning-depth))
      (while (re-search-forward
	      (concat "^\\(\\(?:\\*\\)\\{"
		      (number-to-string (+ (if odd 2 1) level))
		      "\\}\\) \\(.*\\)$")
	      ;; make sure that there is no upper heading
	      (when (> level 0)
		(save-excursion
		  (save-match-data
		    (re-search-forward
		     (concat "^\\(\\(?:\\*\\)\\{"
			     (number-to-string level)
			     "\\}\\) \\(.*\\)$") nil t)))) t)
	(setq cnt (1+ cnt))
	(let* ((pos (match-beginning 0))
	       (heading (match-string 2))
	       (nlevel (if odd (/ (+ 3 level) 2) (1+ level))))
	  (save-excursion
	    (narrow-to-region
	     (point)
	     (save-match-data
	       (if (re-search-forward
		    (concat "^\\(\\(?:\\*\\)\\{"
			    (number-to-string (+ (if odd 2 1) level))
			    "\\}\\) \\(.*\\)$") nil t)
		   (match-beginning 0)
		 (point-max))))
	    (goto-char (point-min))
	    (setq output
		  (append output
			  (list
			   (list
			    `(pos . ,pos)
			    `(level . ,nlevel)
			    `(occur . ,cnt)
			    `(heading . ,heading)
			    `(content . ,(org-export-latex-parse-content))
			    `(subcontent . ,(org-export-latex-parse-subcontent 
					     level odd)))))))
	  (widen)))
      (list output))))

(defun org-export-latex-parse-list (&optional delete)
  "Parse the list at point.
Return a list containing first level items as strings and
sublevels as list of strings."
  (let ((start (point))
	;; Find the end of the list
	(end (save-excursion 
	       (catch 'exit
		 (while (or (looking-at org-export-latex-list-beginning-re)
			    (looking-at "^[ \t]+\\|^$"))
		   (if (eq (point) (point-max))
		       (throw 'exit (point-max)))
		   (forward-line 1))) (point)))
	output itemsep)
    (while (re-search-forward org-export-latex-list-beginning-re end t)
      (setq itemsep (if (save-match-data
			  (string-match "^[0-9]" (match-string 2)))
			"[0-9]+\\(?:\\.\\|)\\)" "[-+]"))
      (let* ((indent1 (match-string 1))
	     (nextitem (save-excursion 
			 (save-match-data
			   (or (and (re-search-forward 
				     (concat "^" indent1 itemsep " *?") end t)
				    (match-beginning 0)) end))))
	     (item (buffer-substring
		    (point)
		    (or (and (re-search-forward 
			      org-export-latex-list-beginning-re end t)
			     (goto-char (match-beginning 0)))
			(goto-char end))))
	     (nextindent (match-string 1))
	     (item (org-trim item))
	     (item (if (string-match "^\\[.+\\]" item)
		       (replace-match "\\\\texttt{\\&}"
				      t nil item) item)))
	(push item output)
	(when (> (length nextindent)
		 (length indent1))
	  (narrow-to-region (point) nextitem)
	  (push (org-export-latex-parse-list) output)
	  (widen))))
    (when delete (delete-region start end))
    (setq output (nreverse output))
    (push (if (string-match "^\\[0" itemsep)
	      'ordered 'unordered) output)))

(defun org-export-latex-parse-content ()
  "Extract the content of a section."
  (let ((beg (point))
	(end (if (re-search-forward "^\\(\\*\\)+ .*$" nil t)
		 (progn (beginning-of-line) (point))
	       (point-max))))
    (buffer-substring beg end)))

(defun org-export-latex-parse-subcontent (level odd)
  "Extract the subcontent of a section at LEVEL.
If ODD Is non-nil, assume subcontent only contains odd sections."
  (if (not (re-search-forward
	    (concat "^\\(\\(?:\\*\\)\\{"
		    (number-to-string (+ (if odd 4 2) level))
		    "\\}\\) \\(.*\\)$")
	    nil t))
      nil ; subcontent is nil
    (org-export-latex-parse-global (+ (if odd 2 1) level) odd)))

;;; Rendering functions:
(defun org-export-latex-global (content)
  "Export CONTENT to LaTeX.
CONTENT is an element of the list produced by
`org-export-latex-parse-global'."
  (if (eq (car content) 'subcontent)
      (mapc 'org-export-latex-sub (cdr content))
    (org-export-latex-sub (car content))))

(defun org-export-latex-sub (subcontent)
  "Export the list SUBCONTENT to LaTeX.
SUBCONTENT is an alist containing information about the headline
and its content."
  (let ((num (plist-get org-latex-options-plist :section-numbers)))
    (mapc (lambda(x) (org-export-latex-subcontent x num)) subcontent)))

(defun org-export-latex-subcontent (subcontent num)
  "Export each cell of SUBCONTENT to LaTeX."
  (let ((heading (org-export-latex-fontify-headline
		  (cdr (assoc 'heading subcontent))))
	(level (- (cdr (assoc 'level subcontent))
		  org-latex-add-level))
	(occur (number-to-string (cdr (assoc 'occur subcontent))))
	(content (cdr (assoc 'content subcontent)))
	(subcontent (cadr (assoc 'subcontent subcontent))))
    (cond 
     ;; Normal conversion
     ((<= level org-latex-sectioning-depth)
      (let ((sec (assoc level org-export-latex-sectioning-alist)))
	(insert (format (if num (cadr sec) (caddr sec)) heading) "\n"))
      (insert (org-export-latex-content content))
      (cond ((stringp subcontent) (insert subcontent))
	    ((listp subcontent) (org-export-latex-sub subcontent))))
     ;; At a level under the hl option: we can drop this subsection
     ((> level org-latex-sectioning-depth)
      (cond ((eq org-export-latex-low-levels 'description)
	     (insert (format "\\begin{description}\n\n\\item[%s]\n\n" heading))
	     (insert (org-export-latex-content content))
	     (cond ((stringp subcontent) (insert subcontent))
		   ((listp subcontent) (org-export-latex-sub subcontent)))
	     (insert "\\end{description}\n"))
	    ((stringp org-export-latex-low-levels)
	     (insert (format org-export-latex-low-levels heading) "\n")
	     (insert (org-export-latex-content content))
	     (cond ((stringp subcontent) (insert subcontent))
		   ((listp subcontent) (org-export-latex-sub subcontent)))))))))


;;; Exporting internals:
(defun org-export-latex-protect-string (string)
  "Prevent further conversion for STRING by adding the
org-protect property."
  (add-text-properties 
   0 (length string) '(org-protected t) string) string)

(defun org-export-latex-protect-char-in-string (char-list string)
  "Add org-protected text-property to char from CHAR-LIST in STRING."
  (with-temp-buffer
    (save-match-data
      (insert string)
      (goto-char (point-min))
      (while (re-search-forward (regexp-opt char-list) nil t)
	(add-text-properties (match-beginning 0)
			     (match-end 0) '(org-protected t)))
      (buffer-string))))

(defun org-export-latex-set-initial-vars (ext-plist)
  "Store org local variables required for LaTeX export.
EXT-PLIST is an optional additional plist."
  (setq org-latex-todo-keywords-1 org-todo-keywords-1
	org-latex-all-targets-regexp
	(org-make-target-link-regexp (org-all-targets))
	org-latex-options-plist
	(org-combine-plists (org-default-export-plist) ext-plist
			    (org-infile-export-plist))
	org-latex-sectioning-depth
	(let ((hl-levels (plist-get org-latex-options-plist :headline-levels))
	      (sec-depth (length org-export-latex-sectioning-alist)))
	      ;; Fall back on org-export-latex-sectioning-alist length if
	      ;; headline-levels goes beyond it
	  (if (> hl-levels sec-depth) sec-depth hl-levels))))

(defun org-export-latex-make-preamble (opt-plist)
  "Make the LaTeX preamble and return it as a string.
Argument OPT-PLIST is the options plist for current buffer."
  (let ((toc (plist-get opt-plist :table-of-contents)))
    (concat 
     (if (plist-get opt-plist :time-stamp-file)
	 (format-time-string "% Created %Y-%m-%d %a %H:%M\n"))
     
     ;; insert LaTeX custom preamble
     org-export-latex-preamble "\n"
     
     ;; insert information on LaTeX packages
     (when org-export-latex-packages-alist
       (mapconcat (lambda(p)
		    (if (equal "" (car p))
			(format "\\usepackage{%s}" (cadr p))
		      (format "\\usepackage[%s]{%s}"
			      (car p) (cadr p))))
		  org-export-latex-packages-alist "\n"))
     
     ;; insert the title
     (format 
      "\\title{%s}\n"
      ;; convert the title
      (org-export-latex-content
       (or (plist-get opt-plist :title)
	   (and (not
		 (plist-get opt-plist :skip-before-1st-heading))
		(org-export-grab-title-from-buffer))
	   (and buffer-file-name
		(file-name-sans-extension
		 (file-name-nondirectory buffer-file-name)))
	   "UNTITLED")))
     
     ;; insert author info
     (if (plist-get opt-plist :author-info)
	 (format "\\author{%s}\n" 
		 (or (plist-get opt-plist :author) user-full-name))
       (format "%%\\author{%s}\n"
	       (or (plist-get opt-plist :author) user-full-name)))
     
     ;; insert the date
     (format "\\date{%s}\n"
	     (format-time-string 
	      (or (plist-get opt-plist :date)
		  org-export-latex-date-format)))
     
     ;; beginning of the document
     "\n\\begin{document}\n\n"
     
     ;; insert the title command
     (if (string-match "%s" org-export-latex-title-command)
	 (format org-export-latex-title-command
		 (plist-get opt-plist :title))
       org-export-latex-title-command)
     "\n\n"
     
     ;; table of contents
     (when (and org-export-with-toc 
		(plist-get opt-plist :section-numbers))
       (cond ((numberp toc)
	      (format "\\setcounter{tocdepth}{%s}\n\\tableofcontents\n\n"
		      (min toc (plist-get opt-plist :headline-levels))))
	     (toc (format "\\setcounter{tocdepth}{%s}\n\\tableofcontents\n\n"
			  (plist-get opt-plist :headline-levels))))))))

(defun org-export-latex-first-lines (&optional comments)
  "Export the first lines before first headline.
COMMENTS is either nil to replace them with the empty string or a
formatting string like %%%%s if we want to comment them out."
  (save-excursion
    (goto-char (point-min))
    (let* ((pt (point))
	   (end (if (and (re-search-forward "^\\*" nil t)
			 (not (eq pt (match-beginning 0))))
		    (goto-char (match-beginning 0))
		  (goto-char (point-max)))))
      (org-export-latex-content
       (org-cleaned-string-for-export
	(buffer-substring (point-min) end)
	:for-LaTeX t
	:emph-multiline t
	:add-text nil
	:comments nil
	:skip-before-1st-heading nil
	:LaTeX-fragments nil)))))

(defun org-export-latex-keywords-maybe (remove-list)
  "Maybe remove keywords depending on rules in REMOVE-LIST."
  (goto-char (point-min))
  (let ((re-todo (mapconcat 'identity org-latex-todo-keywords-1 "\\|")))
    ;; convert TODO keywords
    (when (re-search-forward (concat "^\\(" re-todo "\\)") nil t)
      (if (plist-get remove-list :todo)
	  (replace-match "")
	(replace-match (format "\\texttt{%s}" (match-string 1)) t t)))
    ;; convert priority string
    (when (re-search-forward "\\[\\\\#.\\]" nil t)
      (if (plist-get remove-list :priority)
	  (replace-match "")
	(replace-match (format "\\texttt{%s}" (match-string 0)) t t)))
    ;; convert tags
    (when (re-search-forward "\\(:[a-zA-Z0-9]+\\)+:" nil t)
      (if (or (not org-export-with-tags)
	      (plist-get remove-list :tags))
	  (replace-match "")
	(replace-match (format "\\texttt{%s}" (match-string 0)) t t)))))

(defun org-export-latex-fontify-headline (headline)
  "Fontify special words in a HEADLINE."
  (with-temp-buffer
    ;; FIXME: org-inside-LaTeX-fragment-p doesn't work when the $...$ is at
    ;; the beginning of the buffer - inserting "\n" is safe here though.
    (insert "\n" headline)
    (goto-char (point-min))
    (when (plist-get org-latex-options-plist :emphasize)
      (org-export-latex-fontify))
    (org-export-latex-special-chars
     (plist-get org-latex-options-plist :sub-superscript))
    (org-export-latex-keywords-maybe
     org-export-latex-remove-from-headlines)
    (org-export-latex-links)
    (org-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-export-latex-content (content)
  "Convert CONTENT string to LaTeX."
  (with-temp-buffer
   (insert content)
   (org-export-latex-quotation-marks)
   (when (plist-get org-latex-options-plist :emphasize)
     (org-export-latex-fontify))
   (org-export-latex-special-chars
    (plist-get org-latex-options-plist :sub-superscript))
   (org-export-latex-links)
   (org-export-latex-keywords
    (plist-get org-latex-options-plist :timestamps))
   (org-export-latex-lists)
   (org-export-latex-tables
    (plist-get org-latex-options-plist :tables))
   (org-export-latex-fixed-width
    (plist-get org-latex-options-plist :fixed-width))
   ;; return string
   (buffer-substring (point-min) (point-max))))

(defun org-export-latex-quotation-marks ()
  "Export question marks depending on language conventions.
Local definition of the language overrides
`org-export-latex-quotation-marks-convention' which overrides
`org-export-default-language'."
  (let* ((lang (plist-get org-latex-options-plist :language))
	 (quote-rpl (if (equal lang "fr")
			'(("\\(\\s-\\)\"" "«~")
			  ("\\(\\S-\\)\"" "~»")
			  ("\\(\\s-\\)'" "`"))
		      '(("\\(\\s-\\)\"" "``")
			("\\(\\S-\\)\"" "''")
			("\\(\\s-\\)'" "`")))))
    (mapc (lambda(l) (goto-char (point-min))
	    (while (re-search-forward (car l) nil t)
	      (let ((rpl (concat (match-string 1) (cadr l))))
		(org-export-latex-protect-string rpl)
		(org-if-unprotected
		 (replace-match rpl t t))))) quote-rpl)))

;; | chars/string in Org   | normal environment    | math environment      |
;; |-----------------------+-----------------------+-----------------------|
;; | & # % $               | \& \# \% \$           | \& \# \% \$           |
;; | { } _ ^ \             | \{ \} \_ \^ \\        | {  }  _  ^ \          |
;; |-----------------------+-----------------------+-----------------------|
;; | a_b and a^b           | $a_b$ and $a^b$       | a_b and a^b           |
;; | a_abc and a_{abc}     | $a_a$bc and $a_{abc}$ | a_abc and a_{abc}     |
;; | \tau and \mu          | $\tau$ and $\mu$      | \tau and \mu          |
;; |-----------------------+-----------------------+-----------------------|
;; | \_ \^                 | \_  \^                | \_  \^                |
;; | \(a=\mu\mbox{m}\)     | \(a=\mu\mbox{m}\)     | \(a=\mu\mbox{m}\)     |
;; | \[\beta^2-a=0\]       | \[\beta^2-a=0\]       | \[\beta^2-a=0\]       |
;; | $x=22\tau$            | $x=22\tau$            | $x=22\tau$            |
;; | $$\alpha=\sqrt{a^3}$$ | $$\alpha=\sqrt{a^3}$$ | $$\alpha=\sqrt{a^3}$$ |

(defun org-export-latex-special-chars (sub-superscript)
  "Export special characters to LaTeX.
If SUB-SUPERSCRIPT is non-nil, convert \\ and ^.
See the `org-export-latex.el' code for a complete conversion table."
  (goto-char (point-min))
  (mapc (lambda(c)
	  (goto-char (point-min))
	  (while (re-search-forward c nil t)
	    ;; Put the point where to check for org-protected
	    (unless (get-text-property (match-beginning 2) 'org-protected)
	      (cond ((member (match-string 2) '("\\$" "$"))
		     (if (equal (match-string 2) "\\$")
			 (replace-match (concat (match-string 1) "$"
						(match-string 3)) t t)
		       (replace-match (concat (match-string 1) "\\$"
					      (match-string 3)) t t)))
		    ((member (match-string 2) '("&" "%" "#"))
		     (if (equal (match-string 1) "\\")
			 (replace-match (match-string 2) t t)
		       (replace-match (concat (match-string 1) "\\"
					      (match-string 2)) t t)))
		    ((equal (match-string 2) "~")
		     (cond ((equal (match-string 1) "\\") nil)
			   ((eq 'org-link (get-text-property 0 'face (match-string 2)))
			    (replace-match (concat (match-string 1) "\\~") t t))
			   (t (replace-match 
			       (org-export-latex-protect-string
				(concat (match-string 1) "\\~{}")) t t))))
		    ((member (match-string 2) '("{" "}"))
		     (unless (save-match-data (org-inside-LaTeX-fragment-p))
		       (if (equal (match-string 1) "\\")
			   (replace-match (match-string 2) t t)
			 (replace-match (concat (match-string 1) "\\"
						(match-string 2)) t t)))))
	      (unless (save-match-data (org-inside-LaTeX-fragment-p))
		(cond ((equal (match-string 2) "\\")
		       (replace-match (or (save-match-data
					    (org-export-latex-treat-backslash-char
					     (match-string 1)
					     (match-string 3))) "") t t))
		      ((member (match-string 2) '("_" "^"))
		       (replace-match (or (save-match-data
					    (org-export-latex-treat-sub-super-char
					     sub-superscript
					     (match-string 1)
					     (match-string 2)
					     (match-string 3))) "") t t)))))))
	'("^\\([^\n$]*?\\|^\\)\\(\\\\?\\$\\)\\([^\n$]*\\)$"
 	  "\\([a-za-z0-9]+\\|[ \t\n]\\|\\b\\|\\\\\\)\\(_\\|\\^\\)\\([a-za-z0-9]+\\|[ \t\n]\\|[:punct:]\\|{[a-za-z0-9]+}\\|([a-za-z0-9]+)\\)"
	  "\\(.\\|^\\)\\(\\\\\\)\\([ \t\n]\\|[a-zA-Z&#%{}\"]+\\)"
	  "\\(.\\|^\\)\\(&\\)"
 	  "\\(.\\|^\\)\\(#\\)"
 	  "\\(.\\|^\\)\\(%\\)"
 	  "\\(.\\|^\\)\\({\\)"
	  "\\(.\\|^\\)\\(}\\)"
	  "\\(.\\|^\\)\\(~\\)"
	  ;; (?\< . "\\textless{}")
	  ;; (?\> . "\\textgreater{}")
	  )))

(defun org-export-latex-treat-sub-super-char
  (subsup string-before char string-after)
  "Convert the \"_\" and \"^\" characters to LaTeX.
SUBSUP corresponds to the ^: option in the #+OPTIONS line.
Convert CHAR depending on STRING-BEFORE and STRING-AFTER."
  (cond ((equal string-before "\\")
	 (concat string-before char string-after))
	;; this is part of a math formula
	((and (string-match "\\S-+" string-before)
	      (string-match "\\S-+" string-after))
	 (cond ((eq 'org-link (get-text-property 0 'face char))
		(concat string-before "\\" char string-after))
	       ((save-match-data (org-inside-LaTeX-fragment-p))
		(if subsup
		    (cond ((eq 1 (length string-after))
			   (concat string-before char string-after))
			  ((string-match "[({]?\\([^)}]+\\)[)}]?" string-after)
			   (format "%s%s{%s}" string-before char 
				   (match-string 1 string-after))))))
	       ((and subsup
		     (> (length string-after) 1)
		     (string-match "[({]?\\([^)}]+\\)[)}]?" string-after))
		(format "$%s%s{%s}$" string-before char
			(match-string 1 string-after)))
	       (subsup (concat "$" string-before char string-after "$"))
	       (t (org-export-latex-protect-string
		   (concat string-before "\\" char "{}" string-after)))))
	(t (org-export-latex-protect-string
	    (concat string-before "\\" char "{}" string-after)))))

(defun org-export-latex-treat-backslash-char (string-before string-after)
  "Convert the \"$\" special character to LaTeX.
The conversion is made depending of STRING-BEFORE and STRING-AFTER."
  (cond ((member (list string-after) org-html-entities)
	 ;; backslash is part of a special entity (like "\alpha")
	 (concat string-before "$\\"
		 (or (cdar (member (list string-after) org-html-entities))
		     string-after) "$"))
	((and (not (string-match "^[ \n\t]" string-after))
	      (not (string-match "[ \t]\\'\\|^" string-before)))
	 ;; backslash is inside a word
	 (org-export-latex-protect-string
	  (concat string-before "\\textbackslash{}" string-after)))
	((not (or (equal string-after "")
		  (string-match "^[ \t\n]" string-after)))
	 ;; backslash might escape a character (like \#) or a user TeX
	 ;; macro (like \setcounter)
	 (org-export-latex-protect-string 
	  (concat string-before "\\" string-after)))
	((and (string-match "^[ \t\n]" string-after)
	      (string-match "[ \t\n]\\'" string-before))
	 ;; backslash is alone, convert it to $\backslash$
	 (org-export-latex-protect-string
	  (concat string-before "\\textbackslash{}" string-after)))
	(t (org-export-latex-protect-string
	    (concat string-before "\\textbackslash{}" string-after)))))

(defun org-export-latex-keywords (timestamps)
  "Convert special keywords to LaTeX.
Regexps are those from `org-latex-special-string-regexps'."
  (let ((rg org-latex-special-string-regexps) r)
    (while (setq r (pop rg))
      (goto-char (point-min))
      (while (re-search-forward (eval r) nil t)
	(if (not timestamps)
	    (replace-match (format "\\\\texttt{%s}" (match-string 0)) t)
	  (replace-match ""))))))
  
(defun org-export-latex-fixed-width (opt)
  "When OPT is non-nil convert fixed-width sections to LaTeX."
  (goto-char (point-min))
  ;; FIXME the search shouldn't be performed on already converted text
  (while (re-search-forward "^[ \t]*:" nil t)
    (if opt
	(progn (goto-char (match-beginning 0))
	       (insert "\\begin{verbatim}\n")
	       (while (looking-at "^\\([ \t]*\\):\\(.*\\)$")
		 (replace-match (concat (match-string 1)
					(match-string 2)) t t)
		 (forward-line))
	       (insert "\\end{verbatim}\n\n"))
      (progn (goto-char (match-beginning 0))
	     (while (looking-at "^\\([ \t]*\\):\\(.*\\)$")
	       (replace-match (concat "%" (match-string 1)
				      (match-string 2)) t t)
	       (forward-line))))))

(defun org-export-latex-lists ()
  "Convert lists to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward org-export-latex-list-beginning-re nil t)
    (beginning-of-line)
    (org-export-list-to-latex
     (org-export-latex-parse-list t))))

(defun org-export-list-to-generic (list params)
  "Convert a LIST parsed through `org-export-latex-parse-list' to other formats.
  
Valid parameters are

:ustart     String to start an unordered list
:uend       String to end an unordered list

:ostart     String to start an ordered list
:oend       String to end an ordered list

:splice     When set to t, return only list body lines, don't wrap
            them into :[u/o]start and :[u/o]end.  Default is nil.

:istart     String to start a list item
:iend       String to end a list item
:isep       String to separate items
:lsep       String to separate sublists"
  (interactive)
  (let* ((p params) sublist
	 (splicep (plist-get p :splice))
	 (ostart  (plist-get p :ostart))
	 (oend  (plist-get p :oend))
	 (ustart  (plist-get p :ustart))
	 (uend  (plist-get p :uend))
	 (istart  (plist-get p :istart))
	 (iend  (plist-get p :iend))
	 (isep  (plist-get p :isep))
	 (lsep  (plist-get p :lsep)))
    (let ((wrapper
	   (cond ((eq (car list) 'ordered)
		  (concat ostart "\n%s" oend "\n"))
		 ((eq (car list) 'unordered)
		  (concat ustart "\n%s" uend "\n"))))
	  rtn)
      (while (setq sublist (pop list))
	(cond ((symbolp sublist) nil)
	      ((stringp sublist)
	       (setq rtn (concat rtn istart sublist iend isep)))
	      (t 
	       (setq rtn (concat rtn   ;; previous list
				 lsep  ;; list separator
				 (org-export-list-to-generic sublist p)
				 lsep  ;; list separator
				 )))))
      (format wrapper rtn))))

(defun org-export-list-to-latex (list)
  "Convert LIST into a LaTeX list."
  (insert 
   (org-export-list-to-generic
    list '(:splicep nil :ostart "\\begin{enumerate}" :oend "\\end{enumerate}"
	                :ustart "\\begin{itemize}" :uend "\\end{itemize}"
                        :istart "\\item " :iend ""
			:isep "\n" :lsep "\n"))
   ;; Add a trailing \n after list conversion
   "\n"))

;; FIXME Use org-export-highlight-first-table-line ?
(defun org-export-latex-tables (insert)
  "Convert tables to LaTeX and INSERT it."
  (goto-char (point-min))
  (while (re-search-forward "^\\([ \t]*\\)|" nil t)
    ;; FIXME really need to save-excursion?
    (save-excursion (org-table-align))
    (let* ((beg (org-table-begin))
	   (end (org-table-end))
	   (raw-table (buffer-substring-no-properties beg end))
	   fnum fields line lines olines gr colgropen line-fmt align)
      (if org-export-latex-tables-verbatim
	  (let* ((tbl (concat "\\begin{verbatim}\n" raw-table
			      "\\end{verbatim}\n")))
	    (apply 'delete-region (list beg end))
	    (insert tbl))
	(progn
	  (setq lines (split-string raw-table "\n" t))
	  (apply 'delete-region (list beg end))
 	  (when org-export-table-remove-special-lines
 	    (setq lines (org-table-clean-before-export lines)))
	  ;; make a formatting string to reflect aligment
	  (setq olines lines)
	  (while (and (not line-fmt) (setq line (pop olines)))
	    (unless (string-match "^[ \t]*|-" line)
	      (setq fields (org-split-string line "[ \t]*|[ \t]*"))
	      (setq fnum (make-vector (length fields) 0))
	      (setq line-fmt 
		    (mapconcat
		     (lambda (x)
		       (setq gr (pop org-table-colgroup-info))
		       (format "%s%%s%s"
			       (cond ((eq gr ':start)
				      (prog1 (if colgropen "|" "")
					(setq colgropen t)))
				     ((eq gr ':startend)
				      (prog1 (if colgropen "|" "|")
					(setq colgropen nil)))
				     (t ""))
			       (if (memq gr '(:end :startend))
				   (progn (setq colgropen nil) "|")
				 "")))
		     fnum ""))))
	  ;; maybe remove the first and last "|"
	  (when (string-match "^\\(|\\)?\\(.+\\)|$" line-fmt)
	    (setq line-fmt (match-string 2 line-fmt)))
	  ;; format alignment
	  (setq align (apply 'format 
			     (cons line-fmt
				   (mapcar (lambda (x) (if x "r" "l"))
					   org-table-last-alignment))))
	  ;; prepare the table to send to orgtbl-to-latex
	  (setq lines
		(mapcar
		 (lambda(elem) 
		   (or (and (string-match "[ \t]*|-+" elem) 'hline)
		       (split-string (org-trim elem) "|" t)))
		 lines))
    	  (when insert
	    (insert (orgtbl-to-latex
		     lines `(:tstart ,(concat "\\begin{tabular}{" align "}")))
		    "\n\n")))))))

(defun org-export-latex-fontify ()
  "Convert fontification to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward org-emph-re nil t)
    ;; The match goes one char after the *string*
    (let ((emph (assoc (match-string 3) 
		       org-export-latex-emphasis-alist)) 
	  rpl)
      (unless (get-text-property (1- (point)) 'org-protected)
	(setq rpl (concat (match-string 1)
			  (format (org-export-latex-protect-char-in-string
				   '("\\" "{" "}") (cadr emph))
				  (match-string 4))
			  (match-string 5)))
	(if (caddr emph) 
	    (setq rpl (org-export-latex-protect-string rpl)))
	(replace-match rpl t t)))
    (backward-char)))

(defun org-export-latex-links ()
  ;; Make sure to use the LaTeX hyperref and graphicx package
  ;; or send some warnings.
  "Convert links to LaTeX."
  (goto-char (point-min))
  (while (re-search-forward org-bracket-link-analytic-regexp nil t)
    (org-if-unprotected
     (goto-char (match-beginning 0))
     (let* ((re-radio org-latex-all-targets-regexp)
	    (remove (list (match-beginning 0) (match-end 0)))
	    (type (match-string 2))
	    (raw-path (match-string 3))
	    (full-raw-path (concat (match-string 1) raw-path))
	    (desc (match-string 5))
	    imgp radiop
	    ;; define the path of the link
	    (path (cond
		   ((member type '("http" "https" "ftp"))
		    (concat type ":" raw-path))
		   ((and re-radio (string-match re-radio raw-path))
 		    (setq radiop t))
		   ((equal type "mailto")
		    (concat type ":" raw-path))
		   ((equal type "file")
		    (if (and (or (org-file-image-p (expand-file-name raw-path))
				 (string-match "\\.eps$" raw-path))
			     (equal desc full-raw-path))
			(setq imgp t)
		      (progn (when (string-match "\\(.+\\)::.+" raw-path)
			       (setq raw-path (match-string 1 raw-path)))
			     (if (file-exists-p raw-path)
				 (concat type "://" (expand-file-name raw-path))
			       (concat type "://" (org-export-directory
						   :LaTeX org-latex-options-plist)
				       raw-path))))))))
       ;; process with link inserting
       (apply 'delete-region remove)
       (cond ((and imgp (plist-get org-latex-options-plist :inline-images))
	      (insert (format "\\includegraphics[%s]{%s}"
			      ;; image option should be set be a comment line
			      org-export-latex-image-default-option
			      (expand-file-name raw-path))))
	     ;; FIXME: what about caption? image properties?
	     (radiop (insert (format "\\hyperref[%s]{%s}" raw-path desc)))
	     (path (insert (format "\\href{%s}{%s}" path desc)))
	     (t (insert "\\texttt{" desc "}")))))))

(defun org-export-latex-cleaned-string 
  ;; FIXME remove commentsp call in org.el and here
  (&optional commentsp)
  "Clean stuff in the LaTeX export."

  ;; Preserve line breaks
  (goto-char (point-min))
  (while (re-search-forward "\\\\\\\\" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
			 '(org-protected t)))

  ;; Convert LaTeX to \LaTeX{}
  (goto-char (point-min))
  (let ((case-fold-search nil) rpl)
    (while (re-search-forward "\\([^+_]\\)LaTeX" nil t)
    (replace-match (org-export-latex-protect-string 
		    (concat (match-string 1) "\\LaTeX{}")) t t)))

  ;; Convert horizontal rules
  (goto-char (point-min))
  (while (re-search-forward "^----+.$" nil t)
    (replace-match (org-export-latex-protect-string "\\hrule") t t))

  ;; Protect LaTeX \commands{...}
  (goto-char (point-min))
  (while (re-search-forward "\\\\[a-zA-Z]+\\(?:\\[.*\\]\\)?{.*}" nil t)
    (add-text-properties (match-beginning 0) (match-end 0)
			 '(org-protected t)))

  ;; Replace radio links
  (goto-char (point-min))
  (while (re-search-forward
	  (concat "<<<?" org-latex-all-targets-regexp 
		  ">>>?\\((INVISIBLE)\\)?") nil t)
    (replace-match
     (org-export-latex-protect-string 
      (format "\\label{%s}%s"(match-string 1)
	      (if (match-string 2) "" (match-string 1)))) t t))

  ;; Delete @<...> constructs
  (goto-char (point-min))
  ;; Thanks to Daniel Clemente for this regexp
  (while (re-search-forward "@<\\(?:[^\"\n]\\|\".*\"\\)*?>" nil t)
    (replace-match ""))

  ;; When converting to LaTeX, replace footnotes
  ;; FIXME: don't protect footnotes from conversion
  (when (plist-get org-latex-options-plist :footnotes)
    (goto-char (point-min))
    (while (re-search-forward "\\[[0-9]+\\]" nil t)
      (when (save-match-data
	      (save-excursion (beginning-of-line)
			      (looking-at "[^:|#]")))
	(let ((foot-beg (match-beginning 0))
	      (foot-end (match-end 0))
	      (foot-prefix (match-string 0))
	      footnote footnote-rpl)
	  (when (and (re-search-forward (regexp-quote foot-prefix) nil t))
	    (replace-match "")
	    (let ((end (save-excursion
			 (if (re-search-forward "^$\\|^#.*$\\|\\[[0-9]+\\]" nil t)
			     (match-beginning 0) (point-max)))))
	      (setq footnote
		    (concat 
		     (org-trim (buffer-substring (point) end)) 
		     ;; FIXME stupid workaround for cases where
		     ;; `org-bracket-link-analytic-regexp' matches
		     ;; }. as part of the link.
		     " "))
	      (delete-region (point) end)))
	  (goto-char foot-beg)
	  (delete-region foot-beg foot-end)
	  (setq footnote-rpl (format "\\footnote{%s}" footnote))
	  (add-text-properties 0 10 '(org-protected t) footnote-rpl)
	  (add-text-properties (1- (length footnote-rpl))
			       (length footnote-rpl)
			       '(org-protected t) footnote-rpl)
	  (insert footnote-rpl))))
    
    ;; Replace footnote section tag for LaTeX
    (goto-char (point-min))
    (while (re-search-forward 
	    (concat "^" footnote-section-tag-regexp) nil t)
      (replace-match ""))))

(provide 'org-export-latex)

;; arch-tag: 23c2b87d-da04-4c2d-ad2d-1eb6487bc3ad
;;; org-export-latex.el ends here
