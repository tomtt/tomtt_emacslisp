;;; sb-itmedia.el --- shimbun backend for ITmedia -*- coding: iso-2022-7bit -*-

;; Copyright (C) 2004, 2005, 2006, 2007 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
;;         Yuuichi Teranishi  <teranisi@gohome.org>
;;         ARISAWA Akihiro    <ari@mbf.sphere.ne.jp>
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

;; Original code was nnshimbun.el written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org>.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'shimbun)
(require 'sb-rss)
(require 'sb-multi)

(luna-define-class shimbun-itmedia (shimbun-multi shimbun-rss) ())

(defvar shimbun-itmedia-group-alist
  `(,@(mapcar
       (lambda (group)
	 (list (concat "news." group) (concat "news_" group)))
       '("bursts" "domestic" "foreign" "products" "technology" "web20"
	 "nettopics" "society" "security" "industry" "research" "sp_amd"))
    ("anchordesk" "anchordesk")
    ("bizid" "bizid")
    ("enterprise" "enterprise")
    ,@(mapcar
       (lambda (group)
	 (list (concat "+D." group) group))
       '("plusd" "mobile" "pcupdate" "lifestyle" "games" "docomo" "au_kddi"
	 "vodafone" "shopping"))
    ,@(mapcar
       (lambda (def)
	 (list (concat "+D.lifestyle.column." (car def))
	       nil (car def) (cdr def)))
       '(("asakura" . "$BKcARNg;N(B")
	 ("takemura" . "$BC]B<>y(B")
	 ("kodera" . "$B>.;{?.NI(B")
	 ("honda" . "$BK\ED2m0l(B")
	 ("nishi" . "$B@>@5(B")
	 ("kobayashi" . "$B$3$P$d$7$f$?$+(B")))))

(defvar shimbun-itmedia-x-face-alist
  '(("\\+D" . "X-Face: #Ur~tK`JhZFFHPEVGKEi`MS{55^~&^0KUuZ;]-@WQ[8\
@,Ex'EeAAE}6xF<K#]pULF@5r24J
 |8/oP)(lCAzF0}.C@@}!k8!Qiz6b{]V")
    ("default" . "X-Face: %JzFW&0lP]xKGl{Bk3\\`yC0zZp|!;\\KT9'rqE2AIk\
R[TQ[*i0d##D=I3|g`2yr@sc<pK1SB
 j`}1YEnKc;U0:^#LQB*})Q}y=45<lIE4q<gZ88e2qS8a@Tys6S")))

(luna-define-method shimbun-groups ((shimbun shimbun-itmedia))
  (mapcar 'car shimbun-itmedia-group-alist))

(luna-define-method shimbun-from-address ((shimbun shimbun-itmedia))
  (let ((group (shimbun-current-group-internal shimbun)))
    (format "ITmedia (%s)"
	    (or (nth 3 (assoc group shimbun-itmedia-group-alist)) group))))

(luna-define-method shimbun-index-url ((shimbun shimbun-itmedia))
  (let* ((group (shimbun-current-group-internal shimbun))
	 (name (nth 1 (assoc group shimbun-itmedia-group-alist))))
    (if name
	(format "http://rss.itmedia.co.jp/rss/2.0/%s.xml" name)
      (format "http://plusd.itmedia.co.jp/lifestyle/column/%s.html"
	      (nth 2 (assoc group shimbun-itmedia-group-alist))))))

(luna-define-method shimbun-headers :around ((shimbun shimbun-itmedia)
					     &optional range)
  (if (string-match "\\.xml\\'" (shimbun-index-url shimbun))
      ;; Use the function defined in sb-rss.el.
      (luna-call-next-method)
    ;; Use the default function defined in shimbun.el.
    (funcall (intern "shimbun-headers"
		     (luna-class-obarray (luna-find-class 'shimbun)))
	     shimbun range)))

(luna-define-method shimbun-get-headers :around ((shimbun shimbun-itmedia)
						 &optional range)
  (if (string-match "\\.xml\\'" (shimbun-index-url shimbun))
      (luna-call-next-method)
    (let ((group (nth 2 (assoc (shimbun-current-group-internal shimbun)
			       shimbun-itmedia-group-alist)))
	  (from (shimbun-from-address shimbun))
	  headers)
      (goto-char (point-min))
      (while (re-search-forward "\
<a[\t\n ]+href=\"\\(/lifestyle/articles/\\([0-9][0-9]\\)\\([01][0-9]\\)/\
\\([0-3][0-9]\\)/news\\([0-9]+\\)\\.html\\)\"[\t\n ]*>[\t\n ]*\\([^<]+\\)"
				nil t)
	(push (shimbun-create-header
	       0 (match-string 6) from
	       (shimbun-make-date-string
		(+ (string-to-number (match-string 2)) 2000)
		(string-to-number (match-string 3))
		(string-to-number (match-string 4)))
	       (concat
		"<20" (match-string 2) (match-string 3) (match-string 4)
		"." (match-string 5) "." group
		".column.lifestyle@itmedia.shimbun.namazu.org>")
	       "" 0 0
	       (concat "http://plusd.itmedia.co.jp" (match-string 1)))
	      headers))
      headers)))

(luna-define-method shimbun-multi-next-url ((shimbun shimbun-itmedia)
					    header url)
  (goto-char (point-min))
  (when (re-search-forward
	 "<b><a href=\"\\([^\"]+\\)\">$B<!$N%Z!<%8(B</a></b>\
\\|<span id=\"next\"><a href=\"\\([^\"]+\\)\">$B<!$N%Z!<%8$X(B</a></span>" nil t)
    (shimbun-expand-url (or (match-string 1) (match-string 2)) url)))

(luna-define-method shimbun-multi-clear-contents ((shimbun shimbun-itmedia)
						  header
						  has-previous-page
						  has-next-page)
  (let (credit)
    (when (and (not has-previous-page)
	       (progn
		 (goto-char (point-min))
		 (re-search-forward "<!--$B"#%/%l%8%C%H(B-->[\t\n ]*" nil t))
	       (looking-at "<p\\( [^\n>]+>[^\n]+</\\)p>"))
      (setq credit (match-string 1))
      (when (string-match "<b>\\[ITmedia\\]</b>" credit)
	(setq credit nil)))
    (when (shimbun-clear-contents shimbun header)
      (when credit
	(goto-char (point-min))
	(insert "<div" credit "div>\n"))
      t)))

(luna-define-method shimbun-clear-contents ((shimbun shimbun-itmedia) header)
  (prog1
      (let ((case-fold-search t) (start))
	(goto-char (point-min))
	(when (and (re-search-forward "\
\\(<h[0-9]>[^<]+</h[0-9]>[^<]*\\(\\(?:<[^>h][^>]*>[^<]*\\)*\\)\\)?<!--BODY-->"
				      nil t)
		   (progn
		     (if (setq start (match-beginning 1))
			 (delete-region (match-beginning 2) (match-end 2))
		       (setq start (match-end 0)))
		     (re-search-forward "<!--BODY ?END-->" nil t)))
	  (delete-region (match-beginning 0) (point-max))
	  (delete-region (point-min) start)
	  ;; Remove anchors to both the next page and the previous page.
	  ;; These anchors are inserted into the head and the tail of the
	  ;; article body.
	  (skip-chars-backward " \t\r\f\n")
	  (forward-line 0)
	  (when (looking-at "<P ALIGN=\"CENTER\"><[AB]")
	    (delete-region (point) (point-max)))
	  (goto-char (point-min))
	  (skip-chars-forward " \t\r\f\n")
	  (when (looking-at "<P ALIGN=\"CENTER\"><[AB]")
	    (delete-region (point-min) (point-at-eol)))
	  t))
    (shimbun-remove-tags "<!-- AD START -->" "<!-- AD END -->")
    (shimbun-remove-tags "\
<IMG [^>]*SRC=\"http:/[^\"]*/\\(ad\\.itmedia\\.co\\.jp\\|\
a1100\\.g\\.akamai\\.net\\)/[^>]+>")
    (shimbun-remove-tags "\
<A [^>]*HREF=\"http:/[^\"]*/\\(ad\\.itmedia\\.co\\.jp\\|\
a1100\\.g\\.akamai\\.net\\)/[^>]+>[^<]*</A>")
    (let ((hankaku (shimbun-japanese-hankaku shimbun)))
      (when (and hankaku (not (memq hankaku '(header subject))))
	(shimbun-japanese-hankaku-buffer t)))))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-itmedia)
						   header)
  (when (re-search-forward "\\([0-9]+\\)$BG/(B\\([0-9]+\\)$B7n(B\\([0-9]+\\)$BF|(B \
\\([0-9]+\\)$B;~(B\\([0-9]+\\)$BJ,(B $B99?7(B" nil t)
    (shimbun-header-set-date
     header
     (shimbun-make-date-string
      (string-to-number (match-string 1))
      (string-to-number (match-string 2))
      (string-to-number (match-string 3))
      (concat (match-string 4) ":" (match-string 5))))))

(provide 'sb-itmedia)

;;; sb-itmedia.el ends here
