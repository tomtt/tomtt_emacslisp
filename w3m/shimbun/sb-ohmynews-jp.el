;;; sb-ohmynews-jp.el --- shimbun backend for OhmyNews Japan -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2006, 2007 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: news

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

(require 'shimbun)
(require 'sb-rss)

(luna-define-class shimbun-ohmynews-jp
		   (shimbun-japanese-newspaper shimbun-rss) ())

(defvar shimbun-ohmynews-jp-url "http://www.ohmynews.co.jp/"
  "Name of the parent url.")

(defvar shimbun-ohmynews-jp-server-name "$B%*!<%^%$%K%e!<%9(B")

(defvar shimbun-ohmynews-jp-content-start
  "<!--emacs-w3m-shimbun-ohmynews-jp-content-start-->")

(defvar shimbun-ohmynews-jp-content-end
  "<!--emacs-w3m-shimbun-ohmynews-jp-content-end-->")

(defvar shimbun-ohmynews-jp-group-table
  '(("news" "$B:G?75-;v(B")
    ("watashi" "$B$o$?$7(B" 100)
    ("life" "$B@83h!&0eNE(B" 110)
    ("shakai" "$B<R2q(B" 120)
    ("seiji" "$B@/<#(B" 130)
    ("keizai" "$B7P:Q(B" 140)
    ("kaigai" "$B3$30(B" 150)
    ("culture" "$B%+%k%A%c!<(B" 160)
    ("style" "$B%9%?%$%k(B" 170)
    ("sports" "$B%9%]!<%D(B" 180)
    ("it" "IT" 190)
    ("local" "$BCO0hHG(B" 200)))

(defvar shimbun-ohmynews-jp-x-face-alist
  '(("default" . "X-Face: o|vpDA-})w*TrtnFk9lZ\",j\"y_kn<xZy+LC\\zH(wC$\
Q^ur1c4B3)t\\tK\\yi-Qku8$*\\d<m]\n x;<6rdcYugs1o1w2dObSQ.INk`9f1x!hNe\\\
v*[xW.y6Tt/r=U{a?+nH20N{)a/w145kJxfhqf}Jd<p\n `bP:u\\Awi^xGQ3pUOrsPL.';\
|}zKE@+4GE4!+rd4[>dSxnHe#Z4#\\hy*R&}uSO=(,5UM)-jERou2]H\n ,5\"$Ka&<hoeL")))

(defvar shimbun-ohmynews-jp-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-ohmynews-jp))
  (mapcar 'car shimbun-ohmynews-jp-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-ohmynews-jp))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-ohmynews-jp-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-ohmynews-jp))
  (let ((index (nth 2 (assoc (shimbun-current-group-internal shimbun)
			     shimbun-ohmynews-jp-group-table))))
    (shimbun-expand-url (if (numberp index)
			    (format "rss/news_c%03d.xml" index)
			  "rss/news.xml")
			(shimbun-url-internal shimbun))))

(luna-define-method shimbun-clear-contents :around ((shimbun
						     shimbun-ohmynews-jp)
						    header)
  (while (search-forward "\r" nil t) ;; There are CRs not followed LFs.
    (delete-region (match-beginning 0) (match-end 0)))
  (goto-char (point-min))
  (when (or (re-search-forward "<\\(?:!-+#+[\t\n ]*top_entry_sub/[\t\n ]*#+-+\
\\|div\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"top_entry_sub\"\
\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]*\\|div[\t\n ]+class=\"title\"\\)>\
\\(?:[\t\n ]*<[^>h][^>]*>\\)*[\t\n ]*<h[^>]*>[^<]+<[^>]+>[\t\n ]*"
			       nil t)
	    (re-search-forward "<!-+#+[\t\n ]*news_tmp_box[^/>]+/[\t\n ]*#+-+>\
\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*<h[^>]*>[^<]+<[^>]+>[\t\n ]*"
			       nil t))
    (insert shimbun-ohmynews-jp-content-start)
    (let ((start (point))
	  end)
      (when (cond ((re-search-forward "\
\\(?:[\t\n $B!!!v(B]\\|&nbsp\;\\|<br>\\|<p\\(?:[\t\n ]+[^>]+\\)?>\\|</p>\\)*\
\\(?:\\(?:<a[\t\n ]+[^>]+>[^<]+</a>\\|<[^>]+>\\)\
\\(?:[\t\n $B!!!v(B]\\|&nbsp\;\\|<br>\\|<p\\(?:[\t\n ]+[^>]+\\)?>\\|</p>\\)*\\)*\
\$B!Z(B[^$B![(B]*$B4XO"(B\\(?:$B%j%s%/(B\\|$B5-;v(B\\)[^$B![(B]*$B![(B"
				      nil t)
		   (goto-char (match-beginning 0))
		   t)
		  ((re-search-forward "\
\\(?:[\t\n $B!!!v(B]\\|&nbsp\;\\|<br>\\|<p\\(?:[\t\n ]+[^>]+\\)?>\\|</p>\\)*\
\\(?:\\(?:\\(?:<a[\t\n ]+[^>]+>[^<]+</a>\\|<[^>]+>\\)\
\\(?:[\t\n $B!!!v(B]\\|&nbsp\;\\|<br>\\|<p\\(?:[\t\n ]+[^>]+\\)?>\\|</p>\\)*\\)*\
\$B!Z(B[^$B![(B]+$B![(B\
\\(?:[\t\n $B!!!v(B]\\|&nbsp\;\\|<br>\\|<p\\(?:[\t\n ]+[^>]+\\)?>\\|</p>\\)*\\)?\
\\(?:\\(?:<a[\t\n ]+[^>]+>[^<]+</a>\\|<[^>]+>\\)\
\\(?:[\t\n $B!!!v(B]\\|&nbsp\;\\|<br>\\|<p\\(?:[\t\n ]+[^>]+\\)?>\\|</p>\\)*\\)*\
<!-+#+[\t\n ]*/\\(?:news_entry_body\\|news_tmp_box[^>]+\\)[\t\n ]*#+-+>"
				      nil t)
		   (goto-char (match-beginning 0))
		   (when (looking-at "\
\\(?:[\t\n $B!!!v(B]\\|&nbsp\;\\|<br>\\|<p\\(?:[\t\n ]+[^>]+\\)?>\\|</p>\\)*\
<a[\t\n ]+[^>]+>[^<]+</a>")
		     (goto-char (match-end 0)))
		   t))
	(setq end (point-marker))
	(insert shimbun-ohmynews-jp-content-end)
	(goto-char start)
	(when (and (re-search-forward "[\t\n ]*<div[\t\n ]+class=\"news_btn\">"
				      end t)
		   (progn
		     (setq start (match-beginning 0))
		     (re-search-forward "<\
\\(?:!-+#*[\t\n ]*news_entry_body/[\t\n ]*#+-+\
\\|div[\t\n ]+class=\"news_entry_body\"\\)>[\t\n ]*"
					end t)))
	  (delete-region start (point))
	  (insert "\n<p>")))))
  (when (luna-call-next-method)
    ;; Break long lines.
    (unless (shimbun-prefer-text-plain-internal shimbun)
      (shimbun-break-long-japanese-lines))
    t))

(provide 'sb-ohmynews-jp)

;;; sb-ohmynews-jp.el ends here
