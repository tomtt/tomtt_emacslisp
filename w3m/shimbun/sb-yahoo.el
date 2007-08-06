;;; sb-yahoo.el --- shimbun backend for news.yahoo.co.jp -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2001, 2002, 2003, 2005, 2006, 2007 Kazuyoshi KOREEDA

;; Author: Kazuyoshi KOREEDA <Koreeda.Kazuyoshi@jp.panasonic.com>,
;;         Katsumi Yamaoka <yamaoka@jpl.org>
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

;; Original code was sb-asahi.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-yahoo (shimbun) ())

(defvar shimbun-yahoo-prefer-text-plain nil
  "*Non-nil means prefer text/plain articles rather than html articles.")

(defvar shimbun-yahoo-url "http://headlines.yahoo.co.jp/")

(defvar shimbun-yahoo-groups-table
  (let* ((s0 "[\t\n\r ]*")
	 (s1 "[\t\n\r ]+")
	 (default (list
		   (concat
		    "<a" s1 "href=\""
		    ;; 1. url
		    "\\(http://headlines\\.yahoo\\.co\\.jp/hl\\?a="
		    ;; 2. serial number
		    "\\("
		    ;; 3. year
		    "\\(20[0-9][0-9]\\)"
		    ;; 4. month
		    "\\([01][0-9]\\)"
		    ;; 5. day
		    "\\([0-3][0-9]\\)"
		    "[^\"]*\\)\\)"
		    "\"" s0 ">" s0
		    ;; 6. subject
		    "\\([^<]+\\)"
		    s0 "</a>\\(?:" s0 "<[^>]+>\\)+" s0 "$B!J(B" s0
		    "\\(?:<a" s1 "[^>]+>" s0 "\\)?"
		    ;; 7. source
		    "\\([^<$B!K(B]+\\)"
		    s0 "\\(?:</a>" s0 "\\)?"
		    s0 "$B!K(B" s0 "-" s0 "\\(?:[^<]+)" s0 "\\)?"
		    ;; 8. hour
		    "\\([012]?[0-9]\\)"
		    s0 "$B;~(B" s0
		    ;; 9. minute
		    "\\([0-5]?[0-9]\\)"
		    s0 "$BJ,(B")
		   1 2 3 4 5 6 7 8 9))
	 (topnews (list
		   (concat
		    "<a" s1 "href=\""
		    ;; 1. url
		    "\\(http://headlines\\.yahoo\\.co\\.jp/hl\\?a="
		    ;; 2. serial number
		    "\\("
		    ;; 3. year
		    "\\(20[0-9][0-9]\\)"
		    ;; 4. month
		    "\\([01][0-9]\\)"
		    ;; 5. day
		    "\\([0-3][0-9]\\)"
		    "[^\"]*\\)\\)"
		    "\"" s0 ">" s0
		    ;; 6. subject
		    "\\([^<]+\\)"
		    s0 "</a>\\(?:" s0 "<[^>]+>\\)*[^<]*)" s0
		    ;; 7. hour
		    "\\([012]?[0-9]\\)"
		    s0 "$B;~(B" s0
		    ;; 8. minute
		    "\\([0-5]?[0-9]\\)"
		    s0 "$BJ,(B" "[^<]*\\(?:<a" s1 "[^>]+>" s0 "\\)?"
		    ;; 9. source
		    "\\([^<$B!K(B]+\\)")
		   1 2 3 4 5 6 9 7 8)))
    `(("topnews" "$B%H%C%W(B" "topnews" ,@topnews)
      ("politics" "$B@/<#(B" "pol" ,@default)
      ("society" "$B<R2q(B" "soci" ,@default)
      ("people" "$B?M(B" "peo" ,@default)
      ("business-all" "$B7P:QAm9g(B" "bus_all" ,@default)
      ("market" "$B;T67(B" "brf" ,@default)
      ("stock" "$B3t<0(B" "biz" ,@default)
      ("industry" "$B;:6H(B" "ind" ,@default)
      ("international" "$B3$30(B" "int" ,@default)
      ("entertainment" "$B%(%s%?!<%F%$%s%a%s%H(B" "ent" ,@default)
      ("sports" "$B%9%]!<%D(B" "spo" ,@default)
      ("computer" "$B%3%s%T%e!<%?(B" "sci" ,@default)
      ("zenkoku" "$BA49q(B" "loc" ,@default)
      ("hokkaido" "$BKL3$F;(B" "hok" ,@default)
      ("aomori" "$B@D?9(B" "l02" ,@default) ;; not "102" but "l02" ;-)
      ("iwate" "$B4d<j(B" "l03" ,@default)
      ("miyagi" "$B5\>k(B" "l04" ,@default)
      ("akita" "$B=)ED(B" "l05" ,@default)
      ("yamagata" "$B;37A(B" "l06" ,@default)
      ("fukushima" "$BJ!Eg(B" "l07" ,@default)
      ("tokyo" "$BEl5~(B" "l13" ,@default)
      ("kanagawa" "$B?@F`@n(B" "l14" ,@default)
      ("chiba" "$B@iMU(B" "l12" ,@default)
      ("saitama" "$B:k6L(B" "l11" ,@default)
      ("ibaraki" "$B0q>k(B" "l08" ,@default)
      ("tochigi" "$BFJLZ(B" "l09" ,@default)
      ("gunma" "$B72GO(B" "l10" ,@default)
      ("yamanashi" "$B;3M|(B" "l19" ,@default)
      ("nagano" "$BD9Ln(B" "l20" ,@default)
      ("niigata" "$B?73c(B" "l15" ,@default)
      ("toyama" "$BIY;3(B" "l16" ,@default)
      ("ishikawa" "$B@P@n(B" "l17" ,@default)
      ("fukui" "$BJ!0f(B" "l18" ,@default)
      ("aichi" "$B0&CN(B" "l23" ,@default)
      ("gifu" "$B4tIl(B" "l21" ,@default)
      ("shizuoka" "$B@E2,(B" "l22" ,@default)
      ("mie" "$B;0=E(B" "l24" ,@default)
      ("osaka" "$BBg:e(B" "l27" ,@default)
      ("hyogo" "$BJ<8K(B" "l28" ,@default)
      ("kyoto" "$B5~ET(B" "l26" ,@default)
      ("shiga" "$B<"2l(B" "l25" ,@default)
      ("nara" "$BF`NI(B" "l29" ,@default)
      ("wakayama" "$BOB2N;3(B" "l30" ,@default)
      ("tottori" "$BD;<h(B" "l31" ,@default)
      ("shimane" "$BEg:,(B" "l32" ,@default)
      ("okayama" "$B2,;3(B" "l33" ,@default)
      ("hiroshima" "$B9-Eg(B" "l34" ,@default)
      ("yamaguchi" "$B;38}(B" "l35" ,@default)
      ("tokushima" "$BFAEg(B" "l36" ,@default)
      ("kagawa" "$B9a@n(B" "l37" ,@default)
      ("ehime" "$B0&I2(B" "l38" ,@default)
      ("kochi" "$B9bCN(B" "l39" ,@default)
      ("fukuoka" "$BJ!2,(B" "l40" ,@default)
      ("saga" "$B:42l(B" "l41" ,@default)
      ("nagasaki" "$BD9:j(B" "l42" ,@default)
      ("kumamoto" "$B7'K\(B" "l43" ,@default)
      ("oita" "$BBgJ,(B" "l44" ,@default)
      ("miyazaki" "$B5\:j(B" "l45" ,@default)
      ("kagoshima" "$B</;yEg(B" "l46" ,@default)
      ("okinawa" "$B2-Fl(B" "oki" ,@default)))
  "Alist of group names, their Japanese translations, index pages,
regexps and numbers.  Where numbers point to the search result in order
of [0]a url, [1]a serial number, [2]a year, [3]a month, [4]a day,
\[5]a subject, [6]a news source, [7]an hour and [8]a minute.")

(defvar shimbun-yahoo-groups
  (mapcar 'car shimbun-yahoo-groups-table))

(defvar shimbun-yahoo-from-address "nobody@example.com")
(defvar shimbun-yahoo-content-start "<!---$B5-;v(B-->\
\\(?:[\t\n ]*<h[0-9][^>]*>[^[<]+</h[0-9]>[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*\
\[^<]+[0-9]$BJ,G[?.(B[^<]*<a[\t\n ]+href=[^>]+>[^<]+</a>\\)?\
\\(?:[\t\n ]*<[^>i][^>]*>\\)*[\t\n ]*")
(defvar shimbun-yahoo-content-end "[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)*\
\\(?:$B:G=*99?7(B:[01]?[0-9]$B7n(B\\|<!---$B%3%V%i%s%I(B-->\\|<!---/$B5-;v(B-->\\)")

(defvar shimbun-yahoo-x-face-alist
  '(("default" . "X-Face: \"Qj}=TahP*`:b#4o_o63:I=\"~wbql=kpF1a>Sp62\
fpAsVY`saZV[b*GqI!u|i|xKPjNh&P=\n R?n}rh38mkp_:')h=Bh:Rk>0pYF\\I?f\\\
PvPs3>/KG:03n47U?FC[?DNAR4QAQxE3L;m!L10OM$-]kF\n YD\\]-^qzd#'{(o2cu,\
\(}CMi|3b9JDQ(^D\\:@DE}d2+0S2G{VS@E*1Og7Vj#35[77\"z9XBq9$1uF$+W\n u")))
(defvar shimbun-yahoo-expiration-days 7)

(luna-define-method shimbun-index-url ((shimbun shimbun-yahoo))
;;;<DEBUG>
;;  (shimbun-yahoo-index-url shimbun))
;;
;;(defun shimbun-yahoo-index-url (shimbun)
;;;</DEBUG>
  (format "%shl?c=%s&t=l"
	  (shimbun-url-internal shimbun)
	  (nth 2 (assoc (shimbun-current-group-internal shimbun)
			shimbun-yahoo-groups-table))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yahoo)
					 &optional range)
;;;<DEBUG>
;;  (shimbun-yahoo-get-headers shimbun range))
;;
;;(defun shimbun-yahoo-get-headers (shimbun range)
;;;</DEBUG>
  (let* ((case-fold-search t)
	 (from "Yahoo!$B%K%e!<%9(B")
	 (group (shimbun-current-group-internal shimbun))
	 (numbers (cdr (assoc group shimbun-yahoo-groups-table)))
	 (jname (pop numbers))
	 (regexp (progn (pop numbers) (pop numbers)))
	 (pages (shimbun-header-index-pages range))
	 (count 0)
	 (index (shimbun-index-url shimbun))
	 id headers start)
    (catch 'stop
      (while t
	(shimbun-remove-tags "<!-+[\t\n ]*$B%"%/%;%9%i%s%-%s%0(B[\t\n ]*-+>"
			     "<!-+[\t\n ]*/$B%"%/%;%9%i%s%-%s%0(B[\t\n ]*-+>")
	(goto-char (point-min))
	(while (re-search-forward regexp nil t)
	  (setq id (concat "<"
			   (save-match-data
			     (shimbun-replace-in-string
			      (match-string (nth 1 numbers))
			      "-" "."))
			   "%" group ".headlines.yahoo.co.jp>"))
	  (if (shimbun-search-id shimbun id)
	      (throw 'stop nil))
	  (push (shimbun-create-header
		 0
		 (match-string (nth 5 numbers))
		 (concat from " (" jname "/" (match-string (nth 6 numbers))
			 ")")
		 (shimbun-make-date-string
		  (string-to-number (match-string (nth 2 numbers)))
		  (string-to-number (match-string (nth 3 numbers)))
		  (string-to-number (match-string (nth 4 numbers)))
		  (format "%02d:%02d"
			  (string-to-number (match-string (nth 7 numbers)))
			  (string-to-number (match-string (nth 8 numbers)))))
		 id "" 0 0
		 (match-string (nth 0 numbers)))
		headers))
	(goto-char (point-min))
	(if (re-search-forward "<a href=\"\\([^\"]+\\)\">$B<!$N%Z!<%8(B</a>" nil t)
	    (shimbun-retrieve-url (prog1
				      (match-string 1)
				    (erase-buffer))
				  t)
	  (if (and (or (not pages)
		       (< (setq count (1+ count)) pages))
		   (re-search-forward "<!-+[\t\n ]*$B2a5n5-;v(B[\t\n ]*-+>" nil t)
		   (progn
		     (setq start (match-end 0))
		     (re-search-forward "<!-+[\t\n ]*/$B2a5n5-;v(B[\t\n ]*-+>"
					nil t))
		   (progn
		     (narrow-to-region start (match-beginning 0))
		     (goto-char start)
		     (or (re-search-forward "<option[\t\n ]+value=\"\
20[0-9][0-9][01][0-9][0-3][0-9]\"[\t\n ]+selected[\t\n ]*>"
					    nil t)
			 (re-search-forward "<option[\t\n ]+value=\"\
20[0-9][0-9][01][0-9][0-3][0-9]\"[\t\n ]*>"
					    nil t)))
		   (re-search-forward "<option[\t\n ]+value=\"\
\\(20[0-9][0-9][01][0-9][0-3][0-9]\\)\"[\t\n ]*>"
				      nil t))
	      (shimbun-retrieve-url (prog1
					(concat index "&d=" (match-string 1))
				      (erase-buffer))
				    t)
	    (throw 'stop nil)))))
    (shimbun-sort-headers headers)))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-yahoo)
						   header)
;;;<DEBUG>
;;  (shimbun-yahoo-prepare-article shimbun header))
;;
;;(defun shimbun-yahoo-prepare-article (shimbun header)
;;;</DEBUG>
  ;; Remove headline.
  (shimbun-remove-tags "<h[0-9][\t\n ]+class=\"yjXL\">" "</h[0-9]>")
  (shimbun-remove-tags
   "<p[\t\n ]+class=\"yjSt\">[^<]*[0-9]+$B;~(B[0-9]+$BJ,G[?.(B" "</p>")
  ;; Remove garbage.
  (when (re-search-forward "\
\[\t\n ]*<p[\t\n ]+class=\"yjSt\">[\t\n ]*$B3HBg<L??(B[\t\n ]*</p>[\t\n ]*"
			   nil t)
    (delete-region (match-beginning 0) (match-end 0)))
  (shimbun-with-narrowed-article
   shimbun
   ;; Fix the picture tag.
   (cond ((re-search-forward "[\t\n ]*<center>[\t\n ]*<font[^>]+>\
\[\t\n ]*$B3HBg<L??(B[\t\n ]*\\(?:<[^>]+>[\t\n ]*\\)+"
			     nil t)
	  (delete-region (match-beginning 0) (match-end 0))
	  (when (and (shimbun-prefer-text-plain-internal shimbun)
		     (looking-at "[^<>]+"))
	    (replace-match "($B<L??(B: \\&)<br>"))
	  (goto-char (point-min)))
	 ((and (shimbun-prefer-text-plain-internal shimbun)
	       (re-search-forward "<img[\t\n ]+[^>]+>\
\\(?:[\t\n ]*<[^>]+>\\)*[\t\n ]*<font[\t\n ]+[^>]+>[\t\n $B!!(B]*\
\\([^<>]+\\)[\t\n ]*</font>"
				  nil t))
	  (if (string-equal (match-string 1) "$B<L??(B")
	      (replace-match "($B<L??(B)<br>")
	    (replace-match "($B<L??(B: \\1)<br>"))))
   (if (shimbun-prefer-text-plain-internal shimbun)
       (require 'sb-text) ;; `shimbun-fill-column'
     ;; Open paragraphs.
     (while (re-search-forward "$B!#(B<br>[\t ]*\n$B!!(B" nil t)
       (replace-match "$B!#(B<br><br>\n$B!!(B"))
     (goto-char (point-min)))
   ;; Correct the Date header and the position of the footer.
   (let (year footer)
     (when (and
	    (setq year (shimbun-header-date header))
	    (string-match " \\(20[0-9][0-9]\\) " year)
	    (progn
	      (setq year (string-to-number (match-string 1 year)))
	      (re-search-forward
	       (eval-when-compile
		 (let ((s0 "[\t\n ]*")
		       (s1 "[\t\n ]+"))
		   (concat
		    "[\t\n $B!!(B]*<div" s1 "align=right>" s0
		    ;; 1. footer
		    "\\("
		    "$B!J(B[^$B!K(B]+$B!K(B" s1 "-" s1
		    ;; 2. month
		    "\\([01]?[0-9]\\)"
		    s0 "$B7n(B" s0
		    ;; 3. day
		    "\\([0-3]?[0-9]\\)"
		    s0 "$BF|(B" s0
		    ;; 4. hour
		    "\\([012]?[0-9]\\)"
		    s0 "$B;~(B" s0
		    ;; 5. minute
		    "\\([0-5]?[0-9]\\)"
		    s0 "$BJ,(B" s0 "$B99?7(B"
		    "\\)"
		    s0 "</div>\\(?:" s0 "<br>\\)*")))
	       nil t)))
       (shimbun-header-set-date
	header
	(shimbun-make-date-string
	 year
	 (string-to-number (match-string 2))
	 (string-to-number (match-string 3))
	 (format "%02d:%02d"
		 (string-to-number (match-string 4))
		 (string-to-number (match-string 5)))))
       (setq footer (match-string 1))
       (delete-region (match-beginning 0) (match-end 0))
       (if (shimbun-prefer-text-plain-internal shimbun)
	   (insert "<br><br>"
		   (make-string (max (- (symbol-value 'shimbun-fill-column)
					(string-width footer))
				     0)
				? )
		   footer "<br>")
	 (insert "<br><br><div align=right>" footer "</div>")
	 ;; Break long Japanese lines.
	 (goto-char (point-min))
	 (while (re-search-forward "<p[^>]*>\\|</p>\\|[$B!"!#!K!W(B]+" nil t)
	   (unless (eolp)
	     (insert "\n"))))))))

(provide 'sb-yahoo)

;;; sb-yahoo.el ends here
