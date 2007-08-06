;;; sb-tech-on.el --- shimbun backend for <http://techon.nikkeibp.co.jp/>

;; Copyright (C) 2007 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: news

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

(luna-define-class shimbun-tech-on (shimbun-rss) ())

(defvar shimbun-tech-on-user-name 'none
  "*User name to log in on Tech-On! with.
If it is nil, you will be prompted for a user name when logging in on
Tech-On! with.  If it is a string, it will be used as a user name and
you will never be prompted.  If it is neither nil nor a string, you
will never log in.  See also `shimbun-tech-on-password'.")

(defvar shimbun-tech-on-password 'none
  "*Password to use to log in on Tech-On! with.
If it is nil, you will be prompted for a password when logging in on
Tech-On! with.  If it is a string, it will be used as a password and
you will never be prompted.  If it is neither nil nor a string, you
will never log in.  See also `shimbun-tech-on-user-name'.")

(defvar shimbun-tech-on-url "http://techon.nikkeibp.co.jp/")

(defvar shimbun-tech-on-group-table
  '(("mobile" "$B%b%P%$%k(B")
    ("bbint" "$B%V%m!<%I%P%s%I!&%$%s%?%U%'!<%9(B")
    ("d-ce" "$B%G%8%?%k2HEE(B")
    ("AT" "Automotive Technology")
    ("edaonline" "EDA Online")
    ("device" "$BEE;RItIJ%F%/%N%m%8(B")
    ("lsi" "LSI$B>pJs6I(B")
    ("silicon" "Silicon Online")
    ("observer" "$B;:6HF08~%*%V%6!<%P(B")
    ("fpd" "FPD International")
    ("mono" "$B$b$N$E$/$j$H(BIT")
    ("embedded" "$BAH$_9~$_3+H/(B")
    ("mecha" "$B5!3#!&%a%+%H%m%K%/%9(B")
    ("MEMS" "MEMS International")
    ("nano" "$B%J%N%F%/!&?7AG:`(B")
    ("carele" "$B%+!<%(%l%/%H%m%K%/%9(B")
    ("board" "$BF|7P%\!<%I>pJs(B")))

(defvar shimbun-tech-on-server-name "Tech-On!")

(defvar shimbun-tech-on-content-start "\
<!-+[\t\n ]*$B"'<L??!u%-%c%W%7%g%s(B[^>]*-+>[\t\n ]*\
\\|\
<!-+[\t\n ]*$B"'5-;vK\J8(B[\t\n ]*-+>[\t\n ]*")

(defvar shimbun-tech-on-content-end "\
\[\t\n ]*<img\\(?:[\t\n ]+\\(?:[^\t\n >s]\\|s[^\t\n >r]\\|sr[^\t\n >c]\\)\
\[^\t\n >]*\\)*\
\[\t\n ]+\\(?:src=\"/img/nocookie\\.gif\"\\|usemap=\"#nocookie\"\\)\
\\|\
\[\t\n ]*<map[\t\n ]+name=\"nocookie\">\
\\|\
\[\t\n ]*<!-+[\t\n ]*$B"'(B.*$B9T%F%-%9%H%"%I3+;O(B[\t\n ]*-+>\
\\|\
\[\t\n ]*<!-+[\t\n ]*$B"'(B\\(?:$B5-;v2<%J%S(B\\|Red Sheriffs\\|$B%U%C%?(B\\)[\t\n ]*-+>")

(defvar shimbun-tech-on-x-face-alist
  '(("default" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAACAAAAAgAgMAAAAOFJJnAAAADFBMVEUAAAB/gP+ttr7///8
 c6BRHAAAAnUlEQVQY02XNPQpCMQwA4NBs9jDvCJ5CXEVv4dJQLyKuHbyCl3i4Cl3EsSA8+l6NoU0
 HMVk+8gsEa2b2DP94rs7DYyCExZIlJCMw6NF7AaI5VZgOQMOtEhQYTOjDXuH7FrU7ZG9W8LlOkuE
 FrPGD0TFnQdlsmSfB240KyYo7F9dxtIrdRbAAln1SHJK2GmQ9ptwOxsTtRawteTrn6QtRz6k/Cwl
 XeQAAAABJRU5ErkJggg==")))

(defvar shimbun-tech-on-expiration-days 7)

(luna-define-method shimbun-groups ((shimbun shimbun-tech-on))
  (mapcar 'car shimbun-tech-on-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-tech-on))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-tech-on-group-table)))

(luna-define-method shimbun-from-address ((shimbun shimbun-tech-on))
  (concat shimbun-tech-on-server-name
	  " (" (shimbun-current-group-name shimbun) ")"))

(luna-define-method shimbun-index-url ((shimbun shimbun-tech-on))
  (concat shimbun-tech-on-url
	  (shimbun-current-group-internal shimbun)
	  "/index.rdf"))

(luna-define-method shimbun-rss-build-message-id ((shimbun shimbun-tech-on)
						  url date)
  (let ((start 0)
	rest)
    (while (string-match "[0-9]+" url start)
      (push (match-string 0 url) rest)
      (setq start (match-end 0)))
    (if rest
	(concat "<" (mapconcat 'identity (nreverse rest) ".")
		"%" (shimbun-current-group-internal shimbun)
		".techon.nikkeibp.co.jp>")
      (error "Cannot find message-id base"))))

(defvar shimbun-tech-on-logged-in nil)

(defun shimbun-tech-on-login ()
  "Log in on Tech-On! with."
  (interactive)
  (when (or (interactive-p)
	    (not shimbun-tech-on-logged-in))
    (let ((user (cond ((stringp shimbun-tech-on-user-name)
		       shimbun-tech-on-user-name)
		      (shimbun-tech-on-user-name
		       nil)
		      (t
		       (condition-case nil
			   (let (inhibit-quit)
			     (read-string "[Tech-On!] User name: "))
			 (quit nil)))))
	  pass)
      (when (and user
		 (not (string-match "\\`[\t ]*\\'" user))
		 (setq pass (cond ((stringp shimbun-tech-on-password)
				   shimbun-tech-on-password)
				  (shimbun-tech-on-password
				   nil)
				  (t
				   (condition-case nil
				       (let (inhibit-quit)
					 (read-passwd "[Tech-On!] Password: "))
				     (quit nil)))))
		 (not (string-match "\\`[\t ]*\\'" pass)))
	(let ((default-enable-multibyte-characters t))
	  (with-temp-buffer
	    (shimbun-retrieve-url
	     (concat "https://techon.nikkeibp.co.jp/login/login.jsp"
		     "?MODE=LOGIN_EXEC"
		     "&USERID=" user
		     "&PASSWORD=" pass)
	     t)
	    (goto-char (point-min))
	    (setq shimbun-tech-on-logged-in
		  (not (re-search-forward "\
\\(?:$B%f!<%6!<L>(B\\|$B%Q%9%o!<%I(B\\).*$B$K8m$j$,$"$j$^$9!#(B\
\\|$B2q0wEPO?$,9T$o$l$F$$$^$;$s!#(B\
\\|ACTION=\"/login/login\\.jsp\\?MODE=LOGIN_EXEC\""
					  nil t)))))
	(if shimbun-tech-on-logged-in
	    (when (interactive-p)
	      (message "[Tech-On!] Logged in"))
	  (when (prog2
		    (message nil)
		    (y-or-n-p "[Tech-On!] Login failed; retry? ")
		  (message nil))
	    (setq shimbun-tech-on-user-name nil
		  shimbun-tech-on-password nil)
	    (shimbun-tech-on-login)))))))

(luna-define-method shimbun-clear-contents :before ((shimbun shimbun-tech-on)
						    header)
  ;; Remove useless images and lines.
  (shimbun-with-narrowed-article
   shimbun
   (when (re-search-forward "\
<!-+[\t\n ]*$B"'<L??!u%-%c%W%7%g%s(B[\t\n ]*[($B!J(B]$B2<2hA|(B[)$B!K(B][\t\n ]*-+>"
			    nil t)
     (let ((start (point)))
       (while (re-search-forward "[\t\n ]*<img[\t\n ]+\\([^>]+\\)>[\t\n ]*"
				 nil t)
	 (when (save-match-data
		 (re-search-backward "src=\"[^\"]+/spacer\\.gif\""
				     (match-beginning 1) t))
	   (delete-region (match-beginning 0) (match-end 0))))
       (goto-char start)
       (while (re-search-forward "[\t\n ]*<hr\\(?:[\t\n ]+[^>]+\\)*>[\t\n ]*"
				 nil t)
	 (delete-region (match-beginning 0) (match-end 0)))))))

(luna-define-method shimbun-article :before ((shimbun shimbun-tech-on)
					     &rest args)
  (shimbun-tech-on-login))

(luna-define-method shimbun-close :after ((shimbun shimbun-tech-on))
  (setq shimbun-tech-on-logged-in nil))

(provide 'sb-tech-on)

;;; sb-tech-on.el ends here
