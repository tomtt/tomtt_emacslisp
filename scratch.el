;; Where random and experimental crap lives

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

;;(partial-completion-mode t)

(defun indent-line-and-line-below ()
  (interactive)
  (c-indent-command)
  (save-excursion
    (move-beginning-of-line 2)
    (c-indent-command)))

(defun indent-line-below ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 2)
    (c-indent-command)))

;;(setq indent-line-function 'indent-line-and-line-below)

(defadvice indent-for-tab-command (after also-indent-line-below ())
  "When indenting current line, also indent the line below it."
  (indent-line-below))
(ad-activate 'indent-for-tab-command)

;; ffap for ruby
(defvar ruby-program-name "ruby")
(defun ruby-module-path(module)
    (shell-command-to-string
     (concat
      ruby-program-name " -e "
      "\"ret='()';$LOAD_PATH.each{|p| "
      "x=p+'/'+ARGV[0].gsub('.rb', '')+'.rb';"
      "ret=File.expand_path(x)"
      "if(File.exist?(x))};printf ret\" "
      module)))

(eval-after-load "ffap"
  '(push '(ruby-mode . ruby-module-path) ffap-alist))

(setq dired-recursive-deletes 'top
  dired-recursive-copies 'top
  dired-dwim-target t)
(add-hook 'dired-mode-hook (lambda ()
                             (local-set-key (kbd "C-c C-r")
                               'wdired-change-to-wdired-mode)))

(defun mac-ae-get-url (event)
  "Open the URL specified by the Apple event EVENT.
Currently the `mailto' and `txmt' schemes are supported."
  (interactive "e")
  (let* ((ae (mac-event-ae event))
         (the-text (mac-ae-text ae))
         (parsed-url (url-generic-parse-url the-text))
         (the-url-type (url-type parsed-url)))
    (case (intern the-url-type)
      (mailto
       (progn
         (url-mailto parsed-url)
         (select-frame-set-input-focus (selected-frame))))
      (txmt
       (let* ((not-used (string-match "txmt://open\\?url=file://\\
([^&]*\\)\\(&line=\\([0-9]*\\)\\)?" the-text))
              (file-name (match-string 1 the-text))
              (lineno (match-string 3 the-text)))
         (if (null file-name)
             (message "Bad txmt URL: %s" the-text)
           (find-file file-name)
           (goto-line (if lineno (string-to-number lineno) 0))
           (select-frame-set-input-focus (selected-frame)))))
      (t (mac-resume-apple-event ae t)))))

(provide 'scratch)

