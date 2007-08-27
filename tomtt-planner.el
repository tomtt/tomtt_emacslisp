(require 'remember-planner)
(require 'planner)
(require 'planner-registry)
(require 'planner-psvn)
(require 'planner-calendar)
(require 'planner-multi)
(require 'planner-cyclic)
(require 'planner-diary)
(require 'planner-timeclock-summary-proj)
(require 'planner-w3m)
(require 'planner-vm)
(require 'planner-deadline)

(setq diary-file "~/created/productivity/diary")
(setq european-calendar-style t)
(add-hook 'diary-display-hook 'fancy-diary-display)

(load "rem2diary")

(planner-registry-initialize)
(setq remember-handler-functions '(remember-planner-append))
(setq remember-annotation-functions planner-annotation-functions)

(setq planner-project "tomtt_planner")
(setq muse-project-alist
           '(("tomtt_planner"
              ("~/created/productivity/plans"           ;; where your Planner pages are located
               :default "TaskPool" ;; use value of `planner-default-page'
               :major-mode planner-mode
               :visit-link planner-visit-link)

              ;; This next part is for specifying where Planner pages
              ;; should be published and what Muse publishing style to
              ;; use.  In this example, we will use the XHTML publishing
              ;; style.

              (:base "planner-xhtml"
                     ;; where files are published to
                     ;; (the value of `planner-publishing-directory', if
                     ;;  you have a configuration for an older version
                     ;;  of Planner)
                     :path "~/public_html/plans"))))

(setq planner-diary-use-diary t)
(planner-diary-insinuate)
(planner-insinuate-calendar)
(setq planner-day-page-template "* Tasks\n\n\n* Schedule\n\n\n* Diary\n\n\n* Notes\n\n")
(eval-after-load "muse-publish"
  '(add-hook 'muse-after-publish-hook
             'planner-calendar-create-today-link))


(global-set-key (kbd "<f9> t") 'planner-create-task-from-buffer)
(setq planner-carry-tasks-forward 0)

;;; Key customizations

;; Use some extra keybindings (that should be default)
(planner-install-extra-context-keybindings)
(planner-install-extra-task-keybindings)
(planner-install-extra-note-keybindings)

;; Don't clobber C-c C-t
(define-key planner-mode-map "\C-c\C-t" #'muse-publish-this-file)

;; The following key bindings are an attempt to make planner stuff
;; globally accessible.
;;(global-set-key "\C-cp"  nil)
(global-set-key "\C-cpa" 'planner-create-high-priority-task-from-buffer) ; insert A-level task
(global-set-key "\C-cpb" 'planner-create-medium-priority-task-from-buffer) ; insert B-level task
(global-set-key "\C-cpc" 'planner-create-low-priority-task-from-buffer) ; insert C-level task
(global-set-key "\C-cpt" 'planner-create-task) ; insert unrelated task
(global-set-key "\C-cp." 'planner-copy-or-move-task) ; move task
(global-set-key "\C-cpd" 'calendar) ; insert diary entry
(global-set-key "\C-cpe" 'planner-deadline-change) ; change deadline
(global-set-key "\C-cpn" 'remember) ; insert note
(global-set-key "\C-cpo" 'my-diary-open) ; open diary
(global-set-key "\C-cpu" 'planner-update-task) ; update task
(global-set-key "\C-cp\C-s" 'planner-search-notes) ; search notes

(setq timeclock-file "~/created/productivity/timelog")