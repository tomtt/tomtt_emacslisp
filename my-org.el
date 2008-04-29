(setq org-remember-templates
      '(("Tasks" ?t "* TODO %?\n  %i\n  %a" "~/created/org/todo.org") ;; (2)
        ("Appointments" ?a "* Appointment: %?\n%^T\n%i\n  %a" "~/created/org/todo.org")))

(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(eval-after-load 'remember
  '(add-hook 'remember-mode-hook 'org-remember-apply-template))
(global-set-key (kbd "C-c r") 'remember) ;; (3)
(global-set-key (kbd "C-c i") 'org-store-link)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode)) ;; (4)
(global-set-key (kbd "C-c a") 'org-agenda)             ;; (5)
(setq org-todo-keywords '("TODO" "STARTED" "HALTED" "WAITING" "DONE")) ;; (6)
(setq org-agenda-include-diary t)                             ;; (7)
(setq org-agenda-include-all-todo t)                          ;; (8)
;;; End of Sacha

(custom-set-variables
 '(org-agenda-files (quote ("~/created/org/todo.org")))
 '(org-default-notes-file "~/created/org/notes.org")
  '(org-agenda-ndays 7)
  '(org-deadline-warning-days 14)
  '(org-agenda-show-all-dates t)
  '(org-agenda-skip-deadline-if-done t)
  '(org-agenda-skip-scheduled-if-done t)
  '(org-agenda-start-on-weekday nil)
  '(org-reverse-note-order t))

;;  ;'(org-fast-tag-selection-single-key (quote expert))
;;  '(org-agenda-custom-commands
;;    (quote (("d" todo "DELEGATED" nil)
;;            ("c" todo "DONE|DEFERRED|CANCELLED" nil)
;;            ("w" todo "WAITING" nil)
;;            ("W" agenda "" ((org-agenda-ndays 21)))
;;     ("A" agenda ""
;;       ((org-agenda-skip-function
;;        (lambda nil
;;              (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
;;              (org-agenda-ndays 1)
;;            (org-agenda-overriding-header "Today's Priority #A tasks: ")))
;;      ("u" alltodo ""
;;      ((org-agenda-skip-function
;;        (lambda nil
;;              (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
;;                                      (quote regexp) "<[^>\n]+>")))
;;            (org-agenda-overriding-header "Unscheduled TODO entries: "))))))
;;  '(org-remember-store-without-prompt t)
;;  '(remember-annotation-functions (quote (org-remember-annotation)))
;;  '(remember-handler-functions (quote (org-remember-handler))))
;;
;; (setq org-todo-keywords
;;        '((sequence "TODO" "STARTED" "|" "DONE" "DELEGATED" "CANCELLED" "WAITING")))