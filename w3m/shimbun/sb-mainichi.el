;;; sb-mainichi.el --- shimbun backend for MSN-Mainichi -*- coding: iso-2022-7bit; -*-

;; Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007
;; Koichiro Ohba <koichiro@meadowy.org>

;; Author: Koichiro Ohba <koichiro@meadowy.org>
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

;; Original code was sb-yomiuri.el which is written by
;; TSUCHIYA Masatoshi <tsuchiya@namazu.org> and
;; Yuuichi Teranishi <teranisi@gohome.org>

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-mainichi (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-mainichi-prefer-text-plain nil
  "*Non-nil means prefer text/plain articles rather than html articles.")

(defvar shimbun-mainichi-top-level-domain "mainichi-msn.co.jp"
  "Name of the top level domain for the MSN-Mainichi INTERACTIVE.")

(defvar shimbun-mainichi-url
  (concat "http://www." shimbun-mainichi-top-level-domain "/")
  "Name of the parent url.")

(defvar shimbun-mainichi-header-regexp-default
  (let ((s0 "[\t\n ]*")
	(s1 "[\t\n ]+"))
    (list
     (concat
      "<a" s1 "href=\"/"
      ;; 1. url
      "\\("
      "\\(?:[^\t\n \"/]+/\\)+news/\\(?:20[0-9][0-9]/\\(?:[01]?[0-9]/\\)?\\)?"
      ;; 2. serial number
      "\\("
      ;; 3. year
      "\\(20[0-9][0-9]\\)"
      ;; 4. month
      "\\([01][0-9]\\)"
      ;; 5. day
      "\\([0-3][0-9]\\)"
      "\\(?:[^\t\n \"./]\\)+\\)"
      "\\.html\\)"
      "[^>]*>" s0
      ;; 6 subject
      "\\([^<]+\\)"
      s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>"
      "\\(?:" s0 "</td>" s0 "<td" s1 "class=\"time\">"
      s0 "<span" s1 "[^>]+>" s0
      ;; 7?. hour
      "\\([012]?[0-9]\\)"
      ":"
      ;; 8?. minute
      "\\([0-5]?[0-9]\\)"
      "\\)?")
     1 2 3 4 5 6 7 8))
  "List of the default regexp used to extract headers and matching numbers.")

;; Emacs 21 needs increasing `max-lisp-eval-depth' and `max-specpdl-size'
;; when compiling `shimbun-mainichi-group-table' for some unknown reason.
(eval-when-compile
  (if (and (not (featurep 'xemacs))
	   (= emacs-major-version 21)
	   (fboundp 'byte-compile-file-form-defvar))
      (fset 'byte-compile-file-form-defvar
	    `(lambda (form)
	       (let ((fn ,(symbol-function 'byte-compile-file-form-defvar)))
		 (unwind-protect
		     (let ((max-lisp-eval-depth 1000)
			   (max-specpdl-size 1000))
		       (funcall fn form))
		   (fset 'byte-compile-file-form-defvar fn)))))))

(defvar shimbun-mainichi-group-table
  `(("entertainment" "$B%(%s%?!<%F%$%s%a%s%H(B" "about:blank" none)
    ("eye" "$BKhF|$N;kE@(B" "about:blank" none)
    ("eye.shasetsu" "$B<R@b(B" nil
     ,(let ((s0 "[\t\n ]*")
	    (s1 "[\t\n ]+"))
	(concat
	 "<a" s1 "href=\"/"
	 ;; 1. url
	 "\\(eye/shasetsu/\\(?:news\\|archive/news/20[0-9][0-9]/[01][0-9]\\)/"
	 ;; 2. serial number
	 "\\("
	 ;; 3. year
	 "\\(20[0-9][0-9]\\)"
	 ;; 4. month
	 "\\([01][0-9]\\)"
	 ;; 5. day
	 "\\([0-3][0-9]\\)"
	 "\\)"
	 "[^\"]+\\)"
	 "\"[^>]*>" s0
	 ;; 6 subject
	 "\\([^<]+\\)"
	 s0 "\\(?:<img" s1 "[^>]+>" s0 "\\)?</a>"
	 "\\(?:" s0 "</td>" s0 "<td" s1 "class=\"time\">"
	 s0 "<span" s1 "[^>]+>" s0
	 ;; 7?. hour
	 "\\([012]?[0-9]\\)"
	 ":"
	 ;; 8?. minute
	 "\\([0-5]?[0-9]\\)"
	 "\\)?"))
     1 2 3 4 5 6 7 8)
    ("kansai" "$B$a$C$A$c4X@>(B" "about:blank" none)
    ("kansai.tigers" "$B$,$s$P$l!*%?%$%,!<%9(B" "about:blank" none)
    ("keizai" "$B7P:Q!&(BIT" "about:blank" none)
    ("kokusai" "$B9q:](B" "about:blank" none)
    ("kurashi" "$BJk$i$7(B" "about:blank" none)
    ("science" "$B%5%$%(%s%9(B" "about:blank" none)
    ("seiji" "$B@/<#(B" "about:blank" none)
    ("shakai" "$B<R2q(B" "about:blank" none)
    ("shakai.edu" "$B650i(B" "about:blank" none)
    ("shakai.edu.campal" "$B%-%c%s%Q$k(B"
     "http://www.mainichi-msn.co.jp/shakai/edu/campal/archive/")
    ("sokuhou" "$BB.Js(B")
    ("sports" "$B%9%]!<%D(B" "about:blank" none)
    ("sports.column" "$B%3%i%`(B" "about:blank" none)
    ("today" "$B:#F|$NOCBj(B" "rss/wadai.rdf"
     ,(let ((s0 "[\t\n ]*"))
	(concat
	 "<title>" s0
	 ;; 1. subject
	 "\\([^<]+\\)"
	 s0 "</title>" s0 "<link>" s0
	 ;; 2. url
	 "\\([^<]+/"
	 ;; 3. serial number
	 "\\([^./]+\\)"
	 "\\.html\\?in=rssw\\)"
	 s0 "</link>" s0 "<description/>" s0 "<dc:subject>[^<]+</dc:subject>"
	 s0 "<dc:date>" s0
	 ;; 4. year
	 "\\(20[0-9][0-9]\\)"
	 "-"
	 ;; 5. month
	 "\\([01]?[0-9]\\)"
	 "-"
	 ;; 6. day
	 "\\([0-3]?[0-9]\\)"
	 "T"
	 ;; 7. hour
	 "\\([0-2]?[0-9]\\)"
	 ":"
	 ;; 8. minute
	 "\\([0-5]?[0-9]\\)"))
     2 3 4 5 6 1 7 8)
    ("yougo" "$B%K%e!<%9$J8@MU(B"))
  "Alist of group names, their Japanese translations, index pages, regexps
and numbers.  Where numbers point to the regexp search result in order
of [0]a url, [1]a serial number, [2]a year, [3]a month, [4]a day,
\[5]a subject, \[6]an hour and [7]a minute.  If regexp and number are
omitted, the value of `shimbun-mainichi-header-regexp-default' is used
by default.

The value `none' for regexp means there is no header in the group (they
can be found in the subgroups).  In that case, the index page should be
set to \"about:blank\".")

(defvar shimbun-mainichi-subgroups-alist
  (let* ((s0 "[\t\n ]*")
	 (s1 "[\t\n ]+")
	 (kansai
	  (list
	   (concat
	    "<a" s1 "href=\"/"
	    ;; 1. url
	    "\\(kansai\\(?:/[^\"/]+\\)+/archive/news/"
	    "\\(?:20[0-9][0-9]/\\(?:[01][0-9]/\\)?\\)?"
	    ;; 2. serial number
	    "\\("
	    ;; 3. year
	    "\\(20[0-9][0-9]\\)"
	    ;; 4. month
	    "\\([01][0-9]\\)"
	    ;; 5. day
	    "\\([0-3][0-9]\\)"
	    "[^\".]+\\)"
	    "[^\"]+\\)"
	    "[^>]*>" s0
	    ;; 6. subject
	    "\\([^<]+\\)")
	   1 2 3 4 5 6)))
    `(("entertainment"
       ("$B<V(B" "car" "http://www.mainichi-msn.co.jp/entertainment/car/")
       ("$B1G2h(B" "cinema" "http://www.mainichi-msn.co.jp/entertainment/cinema/")
       ("$B%2!<%`(B" "game" "http://www.mainichi-msn.co.jp/entertainment/game/")
       ("$B7]G=(B" "geinou" "http://www.mainichi-msn.co.jp/entertainment/geinou/")
       ("$B8k(B" "igo" "http://www.mainichi-msn.co.jp/entertainment/igo/")
       ("$B%"%K%a!&%^%s%,(B" "manga"
	"http://www.mainichi-msn.co.jp/entertainment/manga/")
       ("$B2;3Z(B" "music" "http://www.mainichi-msn.co.jp/entertainment/music/")
       ("$B>-4}(B" "shougi" "http://www.mainichi-msn.co.jp/entertainment/shougi/")
       ("$B%F%l%S(B" "tv" "http://www.mainichi-msn.co.jp/entertainment/tv/"))
      ("eye"
       ("$B%/%m!<%:%"%C%W(B" "closeup"
	"http://www.mainichi-msn.co.jp/eye/closeup/")
       ("$BH/?.H"(B" "hassinbako" "http://www.mainichi-msn.co.jp/eye/hassinbako/")
       ("$B$R$H(B" "hito" "http://www.mainichi-msn.co.jp/eye/hito/")
       ("$BEZMK2r@b(B" "kaisetsu" "http://www.mainichi-msn.co.jp/eye/kaisetsu/")
       ("$B6a;vJR!9(B" "kinji" "http://www.mainichi-msn.co.jp/eye/kinji/")
       ("$B5-<T$NL\(B" "kishanome" "http://www.mainichi-msn.co.jp/eye/kishanome/")
       ("$B!*(B" "mieru" "http://www.mainichi-msn.co.jp/eye/mieru/")
       ("$B%K%e!<%9#U#P(B" "newsup" "http://www.mainichi-msn.co.jp/eye/newsup/")
       ("$BM>O?(B" "yoroku" "http://www.mainichi-msn.co.jp/eye/yoroku/")
       ("$BM+3ZD"(B" "yuuraku" "http://www.mainichi-msn.co.jp/eye/yuuraku/"))
      ("eye.shasetsu"
       ("$B<R@b(B" nil "http://www.mainichi-msn.co.jp/eye/shasetsu/archive/"
	,@(nthcdr 3 (assoc "eye.shasetsu" shimbun-mainichi-group-table))))
      ("kansai"
       ("21$B@$5*%U%)!<%i%`(B" "21cen.wukansai"
	"http://www.mainichi-msn.co.jp/kansai/wukansai/21cen/archive/"
	,@kansai)
       ("$B$"$J$?$HJb$-$?$$(B" "aruki"
	"http://www.mainichi-msn.co.jp/kansai/aruki/archive/" ,@kansai)
       ("$B855$$,=P$k>&Gd$NOC(B" "genki.salon"
	"http://www.mainichi-msn.co.jp/kansai/salon/genki/archive/" ,@kansai)
       ("$B$U$i$C$H!A=w$R$H$jN9(B" "hitoritabi"
	"http://www.mainichi-msn.co.jp/kansai/hitoritabi/archive/" ,@kansai)
       ("$BnInDH,==H,%+=j!ABg?M$N1sB-(B" "meitei"
	"http://www.mainichi-msn.co.jp/kansai/meitei/archive/" ,@kansai)
       ("$B%U%)%H%8%c!<%J%k(B" "photojournal"
	"http://www.mainichi-msn.co.jp/kansai/photojournal/archive/" ,@kansai)
       ("$B;0;^$N3Z20$X$$$i$C$7$c!A$$!*(B" "sanshi"
	"http://www.mainichi-msn.co.jp/kansai/sanshi/archive/" ,@kansai))
      ("kansai.tigers"
       ("$BG.F.O?(B2007" "netouroku2007"
	"http://www.mainichi-msn.co.jp/kansai/tigers/netouroku2007/archive/"
	,@kansai)
       ("$B%H%i!&$H$T(B" "topics"
	"http://www.mainichi-msn.co.jp/kansai/tigers/topics/archive/"
	,@kansai)
       ("$B%H%i!&%H%i!&%U%#!<%P!<(B" "toratora2"
	"http://www.mainichi-msn.co.jp/kansai/tigers/toratora2/archive/"
	,@kansai)
       ("$B$d$8$&$^%?%$%,!<%9(B2007" "yajiuma2007"
	"http://www.mainichi-msn.co.jp/kansai/tigers/yajiuma2007/archive/"
	,@kansai))
      ("keizai"
       ("IT" "it" "http://www.mainichi-msn.co.jp/keizai/it/")
       ("$B3$30(B" "kaigai" "http://www.mainichi-msn.co.jp/keizai/kaigai/")
       ("$B4k6H(B" "kigyou" "http://www.mainichi-msn.co.jp/keizai/kigyou/")
       ("$B4k6H>pJs(B" "info.kigyou"
	"http://www.mainichi-msn.co.jp/keizai/kigyou/info/")
       ("$B6bM;!&3t(B" "kinyu" "http://www.mainichi-msn.co.jp/keizai/kinyu/")
       ("$B@/:v(B" "seisaku" "http://www.mainichi-msn.co.jp/keizai/seisaku/")
       ("$BOCBj(B" "wadai" "http://www.mainichi-msn.co.jp/keizai/wadai/")
       ("$B7P:Q4QB,(B" "kansoku.wadai"
	"http://www.mainichi-msn.co.jp/keizai/wadai/kansoku/archive/"))
      ("kokusai"
       ("$B%"%U%j%+!&%*%;%"%K%"(B" "afro-ocea"
	"http://www.mainichi-msn.co.jp/kokusai/afro-ocea/")
       ("$BFnKL%"%a%j%+(B" "america"
	"http://www.mainichi-msn.co.jp/kokusai/america/")
       ("$B%"%8%"(B" "asia" "http://www.mainichi-msn.co.jp/kokusai/asia/")
       ("$B%h!<%m%C%Q(B" "europe" "http://www.mainichi-msn.co.jp/kokusai/europe/")
       ("$BCf6aEl!&%m%7%"(B" "mideast"
	"http://www.mainichi-msn.co.jp/kokusai/mideast/"))
      ("kurashi"
       ("$B;R0i$F(B" "bebe"	"http://www.mainichi-msn.co.jp/kurashi/bebe/")
       ("$B%U%!%C%7%g%s(B" "fashion"
	"http://www.mainichi-msn.co.jp/kurashi/fashion/")
       ("$B2HDm(B" "katei" "http://www.mainichi-msn.co.jp/kurashi/katei/")
       ("$B7r9/(B" "kenko" "http://www.mainichi-msn.co.jp/kurashi/kenko/")
       ("$B$3$3$m(B" "kokoro" "http://www.mainichi-msn.co.jp/kurashi/kokoro/")
       ("$B?)(B" "shoku" "http://www.mainichi-msn.co.jp/kurashi/shoku/")
       ("$B<qL#(B" "shumi" "http://www.mainichi-msn.co.jp/kurashi/shumi/")
       ("$BN9(B" "travel" "http://www.mainichi-msn.co.jp/kurashi/travel/")
       ("$B=w$HCK(B" "women" "http://www.mainichi-msn.co.jp/kurashi/women/"))
      ("science"
       ("$B4D6-(B" "env" "http://www.mainichi-msn.co.jp/science/env/")
       ("$B2J3X(B" "kagaku" "http://www.mainichi-msn.co.jp/science/kagaku/")
       ("$B0eNE(B" "medical" "http://www.mainichi-msn.co.jp/science/medical/")
       ("$BM}7OGr=q(B" "rikei" "http://www.mainichi-msn.co.jp/science/rikei/"))
      ("seiji"
       ("$B$=$NB>(B" "feature" "http://www.mainichi-msn.co.jp/seiji/feature/")
       ("$B9T@/(B" "gyousei" "http://www.mainichi-msn.co.jp/seiji/gyousei/")
       ("$B9q2q(B" "kokkai" "http://www.mainichi-msn.co.jp/seiji/kokkai/")
       ("$B@/E^(B" "seitou" "http://www.mainichi-msn.co.jp/seiji/seitou/")
       ("$BA*5s(B" "senkyo" "http://www.mainichi-msn.co.jp/seiji/senkyo/"))
      ("shakai"
       ("$Bk>Js(B" "fu" "http://www.mainichi-msn.co.jp/shakai/fu/")
       ("$B3X7](B" "gakugei" "http://www.mainichi-msn.co.jp/shakai/gakugei/")
       ("$B?M;v(B" "ji" "http://www.mainichi-msn.co.jp/shakai/ji/")
       ("$B;v7o(B" "jiken" "http://www.mainichi-msn.co.jp/shakai/jiken/")
       ("$B9D<<(B" "koushitsu" "http://www.mainichi-msn.co.jp/shakai/koushitsu/")
       ("$BE75$(B" "tenki" "http://www.mainichi-msn.co.jp/shakai/tenki/")
       ("$BOCBj(B" "wadai" "http://www.mainichi-msn.co.jp/shakai/wadai/"))
      ("shakai.edu"
       ("$BK\$HFI=q(B" "book"
	"http://www.mainichi-msn.co.jp/shakai/edu/book/archive/")
       ("'07$BG/%;%s%?!<;n83(B" "center07"
	"http://www.mainichi-msn.co.jp/shakai/edu/jyuken/center07/")
       ("IT$B$N$"$k65<<(B" "class.elearningschool" "\
http://www.mainichi-msn.co.jp/shakai/edu/elearningschool/class/archive/")
       ("$B3X9;$H;d(B" "gakkou"
	"http://www.mainichi-msn.co.jp/shakai/edu/gakkou/archive/")
       ("$B3XNO$H$O2?$+(B" "gakuryoku"
	"http://www.mainichi-msn.co.jp/shakai/edu/gakuryoku/archive/")
       ("$B650i9T@/(B" "gyousei"
	"http://www.mainichi-msn.co.jp/shakai/edu/gyousei/archive/")
       ("$B9b9;(B" "high" "http://www.mainichi-msn.co.jp/shakai/edu/high/archive/")
       ("$BCf3X9;(B" "junior"
	"http://www.mainichi-msn.co.jp/shakai/edu/junior/archive/")
       ("$B<u83!&F~;n(B" "jyuken"
	"http://www.mainichi-msn.co.jp/shakai/edu/jyuken/")
       ("$BKhF|>.3X@8?7J9(B" "maishou"
	"http://www.mainichi-msn.co.jp/shakai/edu/maishou/")
       ("$B%M%C%H<R2q$H;R6!$?$A(B" "morals.net"
	"http://www.mainichi-msn.co.jp/shakai/edu/net/morals/archive/")
       ("$B650i$N?9(B" "mori"
	"http://www.mainichi-msn.co.jp/shakai/edu/mori/archive/")
       ("IT$B$GF~;n$,JQ$o$k(B" "nyushi.elearningschool" "\
http://www.mainichi-msn.co.jp/shakai/edu/elearningschool/nyushi/archive/")
       ("$B>pJs%Q%1%C%H(B" "packet"
	"http://www.mainichi-msn.co.jp/shakai/edu/packet/archive/")
       ("$B>.3X9;(B" "primary"
	"http://www.mainichi-msn.co.jp/shakai/edu/primary/archive/")
       ("$B0i$AD>$7$N2N(B" "sodachi"
	"http://www.mainichi-msn.co.jp/shakai/edu/sodachi/")
       ("$BC10LITB-LdBj(B" "tanni"
	"http://www.mainichi-msn.co.jp/shakai/edu/tanni/archive/")
       ("$BBg3X(B" "university"
	"http://www.mainichi-msn.co.jp/shakai/edu/university/archive/")
       ("$BOCBj(B" "wadai"
	"http://www.mainichi-msn.co.jp/shakai/edu/wadai/archive/"))
      ("shakai.edu.campal"
       ("$BBg3Z?M(B" "dairakujin"
	"http://www.mainichi-msn.co.jp/shakai/edu/campal/dairakujin/archive/")
       ("$BFI8+$7$^$7$?!#(B" "dokumi"
	"http://www.mainichi-msn.co.jp/shakai/edu/campal/dokumi/archive/")
       ("$B>pJsEA8@HD(B" "jouho"
	"http://www.mainichi-msn.co.jp/shakai/edu/campal/jouho/archive/")
       ("$B;B$k(B" "kiru"
	"http://www.mainichi-msn.co.jp/shakai/edu/campal/kiru/archive/")
       ("$B$J$K%3%l!)!*(B" "nanikore"
	"http://www.mainichi-msn.co.jp/shakai/edu/campal/nanikore/archive/")
       ("$B$9$?!&$3$i(B" "sutakora"
	"http://www.mainichi-msn.co.jp/shakai/edu/campal/sutakora/archive/"))
      ("sports"
       ("$B%"%^Ln5e(B" "ama" "http://www.mainichi-msn.co.jp/sports/ama/")
       ("$B3JF.5;(B" "battle" "http://www.mainichi-msn.co.jp/sports/battle/")
       ("$B$=$NB>(B" "feature" "http://www.mainichi-msn.co.jp/sports/feature/")
       ("$BN&>e6%5;(B" "field" "http://www.mainichi-msn.co.jp/sports/field/")
       ("$B6%GO(B" "keiba" "http://www.mainichi-msn.co.jp/sports/keiba/")
       ("$BBg%j!<%0(B" "major" "http://www.mainichi-msn.co.jp/sports/major/")
       ("$B%W%mLn5e(B" "pro" "http://www.mainichi-msn.co.jp/sports/pro/")
       ("$B%5%C%+!<(B" "soccer" "http://www.mainichi-msn.co.jp/sports/soccer/"))
      ("sports.column"
       ("$B%Y%s%A(B" "benchi"
	"http://www.mainichi-msn.co.jp/sports/benchi/archive/")
       ("$B0dEA;R$OHt$V(B" "gene2.keiba"
	"http://www.mainichi-msn.co.jp/sports/keiba/gene2/")
       ("$BIZ=E7=0J;R$NG<F@$N0l8@(B" "hitokoto"
	"http://www.mainichi-msn.co.jp/sports/hitokoto/archive/")
       ("$B:4F#E5IW$ND6%&%k%H%iGO7t(B" "horsenews.keiba"
	"http://www.mainichi-msn.co.jp/sports/keiba/horsenews/")
       ("$B0f:jf{8^O:$NM=A[>e<j$NGO7t%Y%?(B" "jouzu.keiba"
	"http://www.mainichi-msn.co.jp/sports/keiba/jouzu/")
       ("$B%^%j%h%s$N#N#YH/!&5e3&%$%s%5%$%I(B" "mariyon.major"
	"http://www.mainichi-msn.co.jp/sports/major/mariyon/archive/")
       ("$BM=A[>.@b(B" "novel.keiba"
	"http://www.mainichi-msn.co.jp/sports/keiba/novel/archive/")
       ("$B%W%l%9%k!<%`(B" "pressroom"
	"http://www.mainichi-msn.co.jp/sports/pressroom/archive/")
       ("$B>>Bt0l7{$N#V%G!<%?(B" "vdata.keiba"
	"http://www.mainichi-msn.co.jp/sports/keiba/vdata/"))))
  "Alist of parent groups and lists of subgenres and tables for subgroups.
Each table is the same as the `cdr' of the element of
`shimbun-mainichi-group-table'.  If a table is omitted the value of
`shimbun-mainichi-header-regexp-default' is used by default.")

(defvar shimbun-mainichi-server-name "$BKhF|?7J9(B")

(defvar shimbun-mainichi-from-address "nobody@example.com")

(defvar shimbun-mainichi-content-start
  (let ((s0 "[\t\n ]*"))
    (concat "<!--emacs-w3m-shimbun-mainichi-content-start-->\\|</div>" s0
	    "\\(?:<div" s0 "class=\"img_[^\"]+\"" s0 ">" s0 "\\|<p>\\)")))

(defvar shimbun-mainichi-content-end
  (let ((s0 "[\t\n ]*"))
    (concat "\\(?:" s0 "</[^>]+>\\)*" s0
	    "<!--" s0 "||" s0 "/todays_topics" s0 "||-->")))

(defvar shimbun-mainichi-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAABwAAAAcBAMAAACAI8KnAAAABGdBTUEAALGPC/xhBQAAABh
 QTFRFC2zfDGzfEnDgJn3iU5fnj7vvzuH4+vz++nlsOQAAAC90RVh0U29mdHdhcmUAWFYgVmVyc2l
 vbiAzLjEwYStGTG1hc2sgIFJldjogMTIvMjkvOTQbx6p8AAABA0lEQVR4nGWRPW+DMBiED5vuqK0
 60yppVwQ0rFTYZo348poSA3uDzd/vCxNNT14en1/5zgZYEoCF69rk2zhWPR6GADwCl864tsaHFUJ
 dweTJuMcQZZ0kRQxkqnaHik2DbBwdVuPgtPG7WTcuhPdshdM5f7lp4SpyXUPoazu1i6HZpbY6R3a
 ZhAW8ztmZsDxPqf0Cb6zsVzQjJQA/2GNE2OWHbqaQvEggI7wFfOmxk1esLUL2GrJg2yBkrTSDqvB
 eJKmhqtNpttk3sllICskmdbXlGdkPNcd/TIuuvOxcM65IsxvSa2Q79w7V8AfL2u1nY9ZquuiWfK7
 1BSVNQzxF9B+40y/ui1KdNxt0ugAAAAd0SU1FB9QEDQAjJMA7GTQAAAAASUVORK5CYII=")))

(defvar shimbun-mainichi-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-mainichi))
  (mapcar 'car shimbun-mainichi-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-mainichi))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-mainichi-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-mainichi))
  (let* ((group (shimbun-current-group-internal shimbun))
	 (index (nth 2 (assoc group shimbun-mainichi-group-table))))
    (shimbun-expand-url
     (or index
	 (concat (shimbun-subst-char-in-string
		  ?. ?/ (shimbun-current-group-internal shimbun))
		 "/"))
     (shimbun-url-internal shimbun))))

(defun shimbun-mainichi-make-date-string (&rest args)
  "Run `shimbun-make-date-string' with ARGS and fix a day if needed.

\(shimbun-mainichi-make-date-string YEAR MONTH DAY &optional TIME TIMEZONE)"
  (if (equal (nth 3 args) "23:59:59")
      (apply 'shimbun-make-date-string args)
    (save-match-data
      (let* ((ctime (current-time))
	     (date (apply 'shimbun-make-date-string args))
	     (time (shimbun-time-parse-string date))
	     (ms (car time))
	     (ls (cadr time))
	     (system-time-locale "C"))
	(if (or (> ms (car ctime))
		(and (= ms (car ctime))
		     (> ls (cadr ctime))))
	    ;; It should be yesterday's same time.
	    (progn
	      (setq ms (1- ms))
	      (when (< (setq ls (- ls (eval-when-compile
					(- (* 60 60 24) 65536))))
		       0)
		(setq ms (1- ms)
		      ls (+ ls 65536)))
	      (format-time-string "%a, %d %b %Y %R +0900" (list ms ls)))
	  date)))))

(luna-define-method shimbun-get-headers ((shimbun shimbun-mainichi)
					 &optional range)
  (shimbun-mainichi-get-headers shimbun))

(defun shimbun-mainichi-get-headers (shimbun)
  (let* ((case-fold-search t)
	 (group (shimbun-current-group-internal shimbun))
	 (regexp (or (nthcdr 3 (assoc group shimbun-mainichi-group-table))
		     shimbun-mainichi-header-regexp-default))
	 (from (concat (shimbun-server-name shimbun)
		       " (" (shimbun-current-group-name shimbun) ")"))
	 (editorial (when (string-equal group "eye.shasetsu")
		      (list nil)))
	 (subgroups (cdr (assoc group shimbun-mainichi-subgroups-alist)))
	 numbers start serial snum id subgenre url month day subject urls
	 subjects headers header date subgrp)
    (if (eq (car regexp) 'none)
	(setq regexp nil)
      (setq numbers (cdr regexp)
	    regexp (car regexp)))
    (catch 'stop
      ;; The loop for fetching all the articles in the subgroups.
      (while t
	(when regexp
	  (shimbun-strip-cr)
	  ;; Ignore unwanted links.
	  (cond
	   ((and (string-equal group "eye")
		 (progn
		   (goto-char (point-min))
		   (re-search-forward "<td[\t\n ]+class=\"date_md\">" nil t))))
	   (t
	    (goto-char (point-max))
	    (when (re-search-backward "<!--[\t\n ]*||[\t\n ]*\
\\(?:todays_topics\\|/movie_news\\|/photo_news\\|/top_navi\\)[\t\n ]*||-->"
				      nil 'move))))
	  (delete-region (point-min) (point))

	  ;; Remove special sections.
	  (while (and (re-search-forward "\
\[\t\n ]*\\(<!--[\t\n ]*|[\t\n ]*\\)\\(special[\t\n ]*|-->\\)"
					 nil t)
		      (progn
			(setq start (match-beginning 0))
			(re-search-forward
			 (concat (regexp-quote (match-string 1))
				 "/"
				 (regexp-quote (match-string 2))
				 "[\t\n ]*")
			 nil t)))
	    (delete-region start (match-end 0)))
	  (when (and (string-match "$BM}7OGr=q(B" from)
		     (progn
		       (goto-char (point-min))
		       (re-search-forward "\
\[\t\n ]*<[^>]+>[\t\n ]*$BM}7OGr=q(B\\(?:$B$NFC=8(B\\|$B%7%s%](B\\)[\t\n ]*<"
					  nil t)))
	    (delete-region (match-beginning 0) (point-max)))

	  ;; Remove ranking sections.
	  (goto-char (point-min))
	  (while (and (re-search-forward "[\t ]*<div[\t\n ]+class=\"ranking\">"
					 nil t)
		      (progn
			(setq start (match-beginning 0))
			(re-search-forward "</div>[\t\n ]*" nil t)))
	    (delete-region start (match-end 0)))

	  ;; Remove commented sections.
	  (goto-char (point-min))
	  (while (re-search-forward "\
\[\t\n ]*<[\t\n ]*!\\(?:[^<>]*<[^<>]+>\\)+[^<>]+>[\t\n ]*"
				    nil t)
	    (delete-region (match-beginning 0) (match-end 0)))

	  (goto-char (point-min))
	  (while (re-search-forward regexp nil t)
	    (setq serial (match-string (nth 1 numbers)))
	    (when editorial
	      (setq serial
		    (concat serial "."
			    (if (setq snum (assoc serial editorial))
				(number-to-string
				 (setcdr snum (1+ (cdr snum))))
			      (push (setq snum (cons serial 1)) editorial)
			      "1"))))
	    (setq id (concat "<" serial "%"
			     (when subgenre (concat subgenre "."))
			     (save-match-data
			       (if (string-match "\\." group)
				   (mapconcat
				    'identity
				    (nreverse (split-string group "\\."))
				    ".")
				 group))
			     "." shimbun-mainichi-top-level-domain ">")
		  url (match-string (nth 0 numbers))
		  month (string-to-number (match-string (nth 3 numbers)))
		  day (string-to-number (match-string (nth 4 numbers)))
		  subject (match-string (nth 5 numbers)))
	    (when editorial
	      (setq subject (format "%02d/%02d %s" month day subject)))
	    ;; Exclude duplications.
	    (unless (or (if editorial
			    (and (or (or (member subject subjects)
					 (progn
					   (push subject subjects)
					   nil))
				     (member url urls))
				 (progn
				   (setcdr snum (1- (cdr snum)))
				   t))
			  (member url urls))
			(prog1
			    (shimbun-search-id shimbun id)
			  (push url urls)))
	      (push
	       (shimbun-create-header
		0 subject from
		(shimbun-mainichi-make-date-string
		 (string-to-number (match-string (nth 2 numbers)))
		 month day
		 (when (nth 7 numbers)
		   (if (match-beginning (nth 7 numbers))
		       (format
			"%02d:%02d"
			(string-to-number (match-string (nth 6 numbers)))
			(string-to-number (match-string (nth 7 numbers))))
		     "23:59:59")))
		id "" 0 0
		(shimbun-expand-url url shimbun-mainichi-url))
	       headers))))
	(if subgroups
	    (progn
	      (erase-buffer)
	      (setq subgrp (pop subgroups)
		    from (concat (shimbun-server-name shimbun)
				 " (" (car subgrp) ")")
		    subgenre (cadr subgrp))
	      (shimbun-retrieve-url (caddr subgrp))
	      (setq regexp (or (cadddr subgrp)
			       (car shimbun-mainichi-header-regexp-default))
		    numbers (or (cddddr subgrp)
				(cdr shimbun-mainichi-header-regexp-default))))
	  (throw 'stop nil))))
    (prog1
	(setq headers (shimbun-sort-headers headers))
      (while headers
	(setq header (pop headers)
	      date (shimbun-header-date header))
	(when (string-match "23:59:59" date)
	  (shimbun-header-set-date header
				   (replace-match "00:00" nil nil date)))))))

(luna-define-method shimbun-make-contents :before ((shimbun shimbun-mainichi)
						   header)
  (shimbun-mainichi-prepare-article shimbun header))

(defun shimbun-mainichi-prepare-article (shimbun header)
  (shimbun-with-narrowed-article
   shimbun
   ;; Fix the Date header.
   (when (re-search-forward "<p>$BKhF|?7J9!!(B\
\\(20[0-9][0-9]\\)$BG/(B\\([01]?[0-9]\\)$B7n(B\\([0-3]?[0-9]\\)$BF|!!(B\
\\([012]?[0-9]\\)$B;~(B\\([0-5]?[0-9]\\)$BJ,(B\\(?:$B!!!J:G=*99?7;~4V!!(B\
\\([01]?[0-9]\\)$B7n(B\\([0-3]?[0-9]\\)$BF|!!(B\
\\([012]?[0-9]\\)$B;~(B\\([0-5]?[0-9]\\)$BJ,!K(B\\)?\\'"
			    nil t)
     (shimbun-header-set-date
      header
      (shimbun-make-date-string
       (string-to-number (match-string 1))
       (string-to-number (or (match-string 6) (match-string 2)))
       (string-to-number (or (match-string 7) (match-string 3)))
       (format "%02d:%02d"
	       (string-to-number (or (match-string 8) (match-string 4)))
	       (string-to-number (or (match-string 9) (match-string 5)))))))
   (goto-char (point-min))
   (let ((from (shimbun-header-from-internal header))
	 (group (shimbun-current-group-internal shimbun))
	 (subject (shimbun-header-subject header 'no-encode)))
     (cond ((or (string-match "$B6a;vJR!9(B" from)
		(string-match "\\`$B6a;vJR!9!'(B" subject))
	    ;; Shorten paragraph separators.
	    (while (search-forward "</p><p>$B!!!!!!!~(B</p><p>" nil t)
	      (replace-match "<br>$B!!!!!!!~(B<br>")))
	   ((or (string-match "$BM>O?(B" from)
		(string-match "\\`$BM>O?!'(B" subject))
	    ;; Break continuous lines.
	    (while (search-forward "$B"%(B" nil t)
	      (replace-match "$B!#(B<br><br>$B!!(B")))
	   ((string-equal group "kansai")
	    ;; Include images.
	    (let ((start (point-min))
		  (end (point-max)))
	      (widen)
	      (forward-line -3)
	      (when (prog1
			(if (re-search-forward "<img[\t\n ]+src=" start t)
			    (goto-char (match-beginning 0))
			  (goto-char start)
			  nil)
		      (narrow-to-region (point) end))
		(insert "<!--emacs-w3m-shimbun-mainichi-content-start-->"))))
	   ((string-match "$BKhF|>.3X@8?7J9(B" from)
	    ;; Remove ruby.
	    (shimbun-remove-tags "</rb><rp>$B!J(B</rp><rt>"
				 "</rt><rp>$B!K(B</rp></ruby>")
	    (shimbun-remove-tags "<ruby><rb>"))))
   (when (shimbun-prefer-text-plain-internal shimbun)
     ;; Replace images with text.
     (goto-char (point-min))
     (while (re-search-forward "[\t\n ]*<img[\t\n ]+[^>]+>[\t\n ]*" nil t)
       (replace-match "($B<L??(B)")))))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-mainichi)
						    header)
  (when (luna-call-next-method)
    ;; Break long lines.
    (unless (shimbun-prefer-text-plain-internal shimbun)
      (shimbun-break-long-japanese-lines))
    t))

(provide 'sb-mainichi)

;;; sb-mainichi.el ends here
