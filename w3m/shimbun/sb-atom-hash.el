;;; sb-atom-hash.el --- shimbun backend for atom content -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2006 Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>

;; Author: Tsuyoshi CHO <tsuyoshi_cho@ybb.ne.jp>
;; Keywords: shimbun

;; This file is a part of shimbun.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'sb-atom)
(require 'sb-hash)

(eval-and-compile
  (luna-define-class shimbun-atom-hash (shimbun-hash shimbun-atom) ()))

(defvar shimbun-atom-hash-group-path-alist
  '(;; name rss-url type(opt:html is t) content-start(opt) content-end(opt)
    ))

(luna-define-method shimbun-groups ((shimbun shimbun-atom-hash))
  (mapcar 'car shimbun-atom-hash-group-path-alist))

(luna-define-method shimbun-index-url ((shimbun shimbun-atom-hash))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-atom-hash-group-path-alist)))

(luna-define-method shimbun-atom-build-message-id ((shimbun shimbun-atom-hash)
						   url date)
  (concat "<" (md5 url) "%" (shimbun-current-group-internal shimbun) ">"))

(luna-define-method shimbun-hash-update-items ((shimbun shimbun-atom-hash))
  (with-temp-buffer
    (let ((case-fold-search t))
      (shimbun-retrieve-url
       (shimbun-hash-contents-url shimbun) 'no-cache 'no-decode)
      ;; In some atom feeds, LFs might be used mixed with CRLFs.
      (shimbun-strip-cr)
      (insert
       (prog1
	   (decode-coding-string (buffer-string) (shimbun-rss-get-encoding))
	 (erase-buffer)
	 (set-buffer-multibyte t)))
      (shimbun-hash-update-items-impl shimbun))))

(luna-define-method shimbun-hash-update-items-impl
  ((shimbun shimbun-atom-hash))
  (let (xml dc-ns atom-ns content-ns
	    (buf-str (buffer-string)))
    (with-temp-buffer
      (erase-buffer)
      (set-buffer-multibyte t)
      (insert buf-str)
      ;; parse xml : check url and desc
      (setq xml (condition-case err
		    (xml-parse-region (point-min) (point-max))
		  (error
		   (message "Error while parsing %s: %s"
			    (shimbun-hash-contents-url shimbun)
			    (error-message-string err))
		   nil)))
      (when xml
	(setq dc-ns (shimbun-rss-get-namespace-prefix
		     xml "http://purl.org/dc/elements/1.1/")
	      atom-ns (shimbun-rss-get-namespace-prefix
		       xml "http://www.w3.org/2005/Atom"))
	(dolist (entry (shimbun-rss-find-el
			(intern (concat atom-ns "entry")) xml))
	  (let ((url
		 (catch 'url
		   (dolist (link (shimbun-rss-find-el
				  (intern (concat atom-ns "link")) entry))
		     (when (string= (shimbun-atom-attribute-value
				     (intern (concat atom-ns "rel")) link)
				    "alternate")
		       (throw 'url (shimbun-atom-attribute-value
				    (intern (concat atom-ns "href")) link)))))))
	    (when url
	      (let* ((date (or (shimbun-rss-get-date shimbun url)
			       (shimbun-rss-node-text atom-ns 'modified entry)
			       (shimbun-rss-node-text atom-ns 'created entry)
			       (shimbun-rss-node-text atom-ns 'issued entry)
			       (shimbun-rss-node-text dc-ns 'date entry)))
		     (id (shimbun-atom-build-message-id shimbun url date))
		     content)
		;; save contents
		(let (type mode)
		  (dolist (content-node (shimbun-rss-find-el
					 (intern (concat atom-ns "content"))
					 entry))
		    (setq type (or (shimbun-atom-attribute-value
				    (intern (concat atom-ns "type"))
				    content-node)
				   "text/plain")
			  mode (or (shimbun-atom-attribute-value
				    (intern (concat atom-ns "mode"))
				    content-node)
				   ""))
		    (cond
		     ((string-match "xhtml" type)
		      ;; xhtml (type text/xhtml,application/xhtml+xml)
		      (setq content (shimbun-atom-rebuild-node
				     atom-ns 'content entry)))
		     (t
		      ;; text or html(without xhtml)
		      (if (string= "escaped" mode)
			  ;; escaped CDATA
			  (setq content (shimbun-rss-node-text
					 atom-ns 'content entry))
			;; non-escaped, but  "<>& to &xxx;
			(setq content (with-temp-buffer
					(erase-buffer)
					(insert (shimbun-rss-node-text
						 atom-ns 'content entry))
					(shimbun-decode-entities)
					(buffer-string))))))))
		(when (and id content)
		  (shimbun-hash-set-item shimbun id content))))))))))

(luna-define-method shimbun-get-headers :before ((shimbun shimbun-atom-hash)
						 &optional range)
  (shimbun-hash-update-items-impl shimbun))

(luna-define-method shimbun-make-contents ((shimbun shimbun-atom-hash) header)
  (if (nth 2 (assoc (shimbun-current-group-internal shimbun)
		    shimbun-atom-hash-group-path-alist))
      (shimbun-make-html-contents shimbun header)
    (shimbun-make-text-contents shimbun header)))

(luna-define-method shimbun-clear-contents ((shimbun shimbun-atom-hash) header)
  (let ((start (nth 3 (assoc (shimbun-current-group-internal shimbun)
			     shimbun-atom-hash-group-path-alist)))
	(end (nth 4 (assoc (shimbun-current-group-internal shimbun)
			   shimbun-atom-hash-group-path-alist)))
	(case-fold-search t))
    (goto-char (point-min))
    (when (and (stringp start)
	       (re-search-forward start nil t)
	       (progn
		 (setq start (point))
		 (stringp end))
	       (re-search-forward end nil t))
      (delete-region (match-beginning 0) (point-max))
      (delete-region (point-min) start)
      t)))

(defun shimbun-atom-rebuild-node (namespace local-name element)
  (let* ((node (assq (intern (concat namespace (symbol-name local-name)))
		     element))
	 (text (shimbun-atom-compose-tag node))
	 (cleaned-text (if text (shimbun-replace-in-string
				 text "^[ \000-\037\177]+\\|[ \000-\037\177]+$"
				 ""))))
    (if (string-equal "" cleaned-text)
	nil
      cleaned-text)))

(defun shimbun-atom-compose-tag (node)
  (cond
   ((null node)
    "")
   ((listp node)
    (let ((tag (car node))
	  (attributes (car (cdr node)))
	  (children (cdr (cdr node))))
      (concat "<"
	      (symbol-name tag)
	      (if attributes
		  (concat " "
			  (mapconcat '(lambda (attr)
					(concat (symbol-name (car attr))
						"=\"" (cdr attr) "\""))
				     attributes " "))
		"")
	      (if children
		  (concat ">" (mapconcat 'shimbun-atom-compose-tag
					 children "")
			  "</" (symbol-name tag) ">")
		" />"))))
   ((stringp node)
    node)))

(provide 'sb-atom-hash)

;;; sb-atom-hash.el ends here
