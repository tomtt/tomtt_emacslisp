;;; anything.el --- open anything

;; Copyright (C) 2007  Tamas Patrovics

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;
;; Start with M-x anything, narrow the list by typing some pattern,
;; select with up/down/pgup/pgdown, choose with enter, left/right
;; moves between sources. With TAB actions can be selected if the
;; selected candidate has more than one possible actions.
;;
;; Tested on Emacs 22.
;;
;;
;; Thanks to Vagn Johansen for ideas.
;;


;; TODO:
;;
;;   - process status indication
;;
;;   - transformations are not done properly in case of incomplete
;;     lines from async processes, because the incomplete part is
;;     already inserted into the buffer when the trasformation part
;;     begins
;;
;;   - action lists intergrated into type handlers
;;
;;   - results from async sources should appear in the order they are
;;     specified in anything-sources
;;
;;   - async sources doesn't honor digit-shortcut-count
;;

(require 'cl)

;; User Configuration 

;; This is only an example. Customize it to your own taste!
(defvar anything-sources `(((name . "Buffers")
                            (candidates . anything-buffer-list)
                            (action . (("Switch to Buffer" . switch-to-buffer)
                                       ("Kill Buffer" . kill-buffer))))

                           ((name . "File Name History")
                            (candidates . file-name-history)
                            (type . file))

                           ((name . "Files from Current Directory")
                            (init-func . (lambda ()
                                           (setq anything-default-directory
                                                 default-directory)))
                            (candidates . (lambda ()
                                            (directory-files
                                             anything-default-directory)))
                            (type . file))

                           ((name . "Manual Pages")
                            (candidates . ,(progn
                                             (require 'woman)
                                             (woman-file-name "")
                                             (sort (mapcar 'car
                                                           woman-topic-all-completions)
                                                   'string-lessp)))
                            (action . woman)
                            (requires-pattern . 2))

                           ((name . "Complex Command History")
                            (candidates . (lambda ()
                                            (mapcar 'prin1-to-string
                                                    command-history)))
                            (action . (lambda (c)
                                        (eval (read c))))
                            (delayed)))
  "The source of candidates for anything.

Attributes:

- name (mandatory)

  The name of the source. It is also the heading which appears
  above the list of matches from the source. Must be unique.

- candidates (mandatory)

  Specifies how to retrieve candidates from the source. It can
  either be a variable name, a function called with no parameters
  or the actual list of candidates.

  The list must be a list of strings, so it's the responsibility
  of the source to convert candidates to strings if necessary.

  If the candidates have to be retrieved asynchronously (for
  example, by an external command which takes a while to run)
  then the function should start the external command
  asynchronously and return the associated process object.
  Anything will take care of managing the process (receiving the
  output from it, killing it if necessary, etc.).

  Note that currently results from asynchronous sources appear
  last in the anything buffer regardless of their position in
  `anything-sources'.

- action (mandatory)

  A function with one parameter. Called with the selected candidate.

  It can also be a list of (DISPLAY . FUNCTION) pairs. In this
  case an action can be chosen from a list of actions for the
  currently selected candidate (by default with TAB). The DISPLAY
  string is shown in the completions buffer and the FUNCTION is
  invoked when an action is selected. The first action of the
  list is the default.

- type (optional)

  Indicates the type of the items the source returns. See
  variable `anything-candidate-transformers' for more
  information.

- init-func (optional)

  Function called with no parameters when anything is started. It
  is useful for collecting current state information which can be
  used to create the list of candidates later.

  For example, if a source needs to work with the current
  directory then it can store its value here, because later
  anything does its job in the minibuffer and in the
  `anything-buffer' and the current directory can be different
  there.

- delayed (optional)

  Candidates from the source are shown only if the user stops
  typing and is idle for `anything-idle-delay' seconds.

- requires-pattern (optional)

  If present matches from the source are shown only if the
  pattern is not empty. Optionally, it can have an integer
  parameter specifying the required length of input which is
  useful in case of sources with lots of candidates.")


(defvar anything-enable-digit-shortcuts nil
  "*If t then the first nine matches can be selected using
  Ctrl+<number>.")


(defvar anything-candidate-number-limit 50
  "*Do not show more candidates than this limit from inidividual
  sources. It is usually pointless to show hundreds of matches
  when the pattern is empty, because it is much simpler to type a
  few characters to narrow down the list of potential candidates.

  Set it to nil if you don't want this limit.")


(defvar anything-idle-delay 0.5
  "*The user has to be idle for this many seconds, before
  candidates from delayed sources are collected. This is useful
  for sources involving heavy operations (like launching external
  programs), so that candidates from the source are not retrieved
  unnecessarily if the user keeps typing.

  It also can be used to declutter the results anything displays,
  so that results from certain sources are not shown with every
  character typed, only if the user hesitates a bit.")


;; This value is only provided as an example. Customize it to your own
;; taste!
(defvar anything-type-actions
  '((file . (("Find File" . find-file)
             ("Delete File" . (lambda (file)
                                (if (y-or-n-p (format "Really delete file %s? "
                                                      file))
                                    (delete-file file)))))))
  "A list of (TYPE . ACTION) pairs specifying actions for sources
  which have no action defined. See the `action' attribute of
  `anything-sources' for possible action values.")


;; This value is only provided as an example. Customize it to your own
;; taste!
(defvar anything-candidate-transformers
  '((file . anything-transform-files))
  "It's a list of (TYPE . FUNCTION) pairs. The function is called
  with one argument when the completion list from the source is
  built if the type of items the source provides is TYPE. The
  argument is the list of candidates retrieved from the source
  and the function should return a transformed list of items
  which will be used for the actual completion.

  This can be used to transform or remove items from the list of
  candidates.

  The function can also substitue candidates in the returned list
  with (DISPLAY . REAL) pairs. In this case the DISPLAY string is
  shown in the anything buffer, but the REAL one is used as
  action argument when the candidate is selected. This allows a
  more readable presentation for candidates which would otherwise
  be, for example, too long or have a common part shared with
  other candidates which can be safely replaced with an
  abbreviated string for display purposes.

  Note that if the (DISPLAY . REAL) form is used then pattern
  matching is done on the displayed string, not on the real
  value.")


;; This value is only provided as an example. Customize it to your own
;; taste!
(defvar anything-action-transformers
  '((file . anything-transform-file-actions))
  "It's a list of (TYPE . FUNCTION) pairs. The function is called
  with two arguments when the action list from the source is
  assembled. The first argument is the list of actions, the
  second is the current selection. 

  The function should return a transformed action list.

  This can be used to customize the list of actions based on the
  currently selected candidate.")


(defvar anything-map
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "<down>") 'anything-next-line)
    (define-key map (kbd "<up>") 'anything-previous-line)
    (define-key map (kbd "<prior>") 'anything-previous-page)
    (define-key map (kbd "<next>") 'anything-next-page)
    (define-key map (kbd "<right>") 'anything-next-source)
    (define-key map (kbd "<left>") 'anything-previous-source)
    (define-key map (kbd "<RET>") 'anything-exit-minibuffer)
    (define-key map (kbd "C-1") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-2") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-3") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-4") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-5") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-6") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-7") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-8") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "C-9") 'anything-select-with-digit-shortcut)
    (define-key map (kbd "<tab>") 'anything-select-action)
    map)
  "Keymap for anything.")


(defvar anything-header-face 'header-line
  "Face for header lines in the anything buffer.")

;;----------------------------------------------------------------------
;;
;; Predefined Sources
;;
;;----------------------------------------------------------------------

(defvar anything-source-emacs-commands
  `((name . "Emacs Commands") 
    (candidates . ,(let (commands) 
                     (mapatoms (lambda (a) 
                                 (if (commandp a) 
                                     (push (symbol-name a) 
                                           commands)))) 
                     (sort commands 'string-lessp))) 
    (action . (lambda (command-name) 
                (call-interactively (intern command-name)))))
  "Source for completing and invoking Emacs commands.")


(defvar anything-source-locate 
  '((name . "Locate")        
    (candidates . (lambda ()
                    (start-process "locate-process" nil
                                   "locate" "-r" "-i"
                                   anything-pattern)))
    (type . file)
    (requires-pattern . 3))
  "Source for retrieving files matching the current input pattern
  with locate.")


;;----------------------------------------------------------------------

(defconst anything-buffer "*anything*"
  "Buffer showing completions.")

(defvar anything-selection-overlay nil
  "Overlay used to highlight the currently selected file.")

(defvar anything-digit-overlays nil
  "Overlays for digit shortcuts. See `anything-enable-digit-shortcuts'.")

(defvar anything-candidate-cache nil
  "Holds the available candidate withing a single anything invocation.")

(defvar anything-pattern
  "The input pattern used to update the anything buffer.")

(defvar anything-async-processes nil
  "List of information about asynchronous processes managed by anything.")

(defvar anything-digit-shortcut-count 0
  "Number of digit shortcuts shown in the anything buffer.")

(defvar anything-update-hook nil
  "Run after the aything buffer was updated according the new
  input pattern.")

(defvar anything-saved-sources nil
  "Saved value of the original `anything-sources' when the action
  list is shown.")

(defvar anything-saved-selection nil
  "Saved value of the currently selected object when the action
  list is shown.")


(put 'anything 'timid-completion 'disabled)


(defun anything-buffer-list ()
  "Return the list of names of buffers with the `anything-buffer'
and hidden buffers filtered out."
  (remove-if (lambda (name)
               (or (equal name anything-buffer)
                   (eq ?\  (aref name 0))))
             (mapcar 'buffer-name (buffer-list))))
  

(defun anything-check-minibuffer-input ()
  "Extract input string from the minibuffer and check if it needs
to be handled."
  (anything-check-new-input (if (window-minibuffer-p)
                                (minibuffer-contents)
                              "")))


(defun anything-check-new-input (input)
  "Check input string and update the anything buffer if
necessary."
  (unless (equal input anything-pattern)
    (setq anything-pattern input)
    (anything-update)))


(defun anything-update ()
  "Update the list of matches in the anything buffer according to
the current pattern."
  (setq anything-digit-shortcut-count 0)
  (anything-kill-async-processes)
  (with-current-buffer anything-buffer
    (erase-buffer)

    (if anything-enable-digit-shortcuts
        (dolist (overlay anything-digit-overlays)
          (delete-overlay overlay)))

    (let (delayed-sources)
      (dolist (source anything-sources)
        (if (equal anything-pattern "")
            (unless (assoc 'requires-pattern source)
              (if (assoc 'delayed source)
                  (push source delayed-sources)
                (anything-process-source source)))

          (let ((min-pattern-length (cdr (assoc 'requires-pattern source))))
            (unless (and min-pattern-length
                         (< (length anything-pattern) min-pattern-length))
              (if (assoc 'delayed source)
                  (push source delayed-sources)
                (anything-process-source source))))))

      (goto-char (point-min))
      (run-hooks 'anything-update-hook)
      (anything-next-line)

      (run-with-idle-timer 0 nil 'anything-process-delayed-sources
                           delayed-sources))))


(defun anything-process-source (source)
  "Display matches from SOURCE according to its settings."
  (let (matches)
    (if (equal anything-pattern "")
        (progn
          (setq matches (anything-get-cached-candidates source))
          (if (> (length matches) anything-candidate-number-limit)
              (setq matches 
                    (subseq matches 0 anything-candidate-number-limit))))

      (let ((item-count 0))
        (dolist (candidate (anything-get-cached-candidates source))
          (when (string-match anything-pattern (if (listp candidate)
                                                   (car candidate)
                                                 candidate))
            (push candidate matches)

            (when anything-candidate-number-limit
              (incf item-count)
              (if (= item-count anything-candidate-number-limit)
                  (return))))))

      (setq matches (reverse matches)))

    (when matches
      (anything-insert-header (cdr (assoc 'name source)))

      (dolist (match matches)
        (when (and anything-enable-digit-shortcuts
                   (not (eq anything-digit-shortcut-count 9)))
          (move-overlay (nth anything-digit-shortcut-count
                             anything-digit-overlays)
                        (line-beginning-position)
                        (line-beginning-position))
          (incf anything-digit-shortcut-count))

        (anything-insert-match match 'insert)))))


(defun anything-insert-match (match insert-function)
  "Insert MATCH into the anything buffer. If MATCH is a list then
insert the string inteneded to appear on the display and store
the real value in a text property."
  (if (not (listp match))
      (funcall insert-function match)

    (funcall insert-function (car match))
    (put-text-property (line-beginning-position) (line-end-position) 
                       'anything-realvalue (cdr match)))
  (funcall insert-function "\n"))


(defun anything-process-delayed-sources (delayed-sources)
  "Process delayed sources if the user is idle for
`anything-idle-delay' seconds."
  (if (sit-for anything-idle-delay)
      (with-current-buffer anything-buffer        
        (save-excursion
          (goto-char (point-max))
          (dolist (source delayed-sources)
            (anything-process-source source))

          (when (and (not (equal (buffer-size) 0))
                     ;; no selection yet
                     (= (overlay-start anything-selection-overlay)
                        (overlay-end anything-selection-overlay)))
            (goto-char (point-min))
            (run-hooks 'anything-update-hook)
            (anything-next-line))))))


(defun anything ()
  "Select anything."
  (interactive)
  (let ((winconfig (current-window-configuration)))
    (add-hook 'post-command-hook 'anything-check-minibuffer-input)

    (anything-initialize)
    (pop-to-buffer anything-buffer)

    (unwind-protect
        (progn
          (anything-update)
          (let ((minibuffer-local-map anything-map))
            (read-string "pattern: ")))

      (anything-cleanup)
      (remove-hook 'post-command-hook 'anything-check-minibuffer-input)
      (set-window-configuration winconfig)))

  (anything-execute-selection-action))


(defun anything-execute-selection-action ()
  "If a candidate was selected then perform the associated
action."
  (let* ((selection (if anything-saved-sources
                        ;; the action list is shown
                        anything-saved-selection
                      (anything-get-selection)))
         (action (if anything-saved-sources
                     ;; the action list is shown
                     (anything-get-selection)
                   (anything-get-action))))

    (if (anything-list-but-not-lambda-p action)
        ;; select the default action
        (setq action (cdar action)))

    (if (and selection action)
        (funcall action selection))))


(defun anything-list-but-not-lambda-p (x)
  "Return t if x is a list, but not a lambda function."
  (and (listp x)       
       (not (functionp x))))


(defun anything-get-selection ()
  "Return the currently selected item or nil."
  (unless (= (buffer-size (get-buffer anything-buffer)) 0)
    (with-current-buffer anything-buffer
      (let ((selection
             (or (get-text-property (overlay-start
                                     anything-selection-overlay)
                                    'anything-realvalue)
                 (buffer-substring-no-properties
                  (overlay-start anything-selection-overlay)
                  (1- (overlay-end anything-selection-overlay))))))
        (unless (equal selection "")
          selection)))))


(defun anything-get-action ()
  "Return the associated action for the selected candidate."
  (unless (= (buffer-size (get-buffer anything-buffer)) 0)
    (with-current-buffer anything-buffer
      (let* ((header-pos (anything-get-previous-header-pos))
             (source-name
              (save-excursion
                (assert header-pos)
                (goto-char header-pos)
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
             (source (some (lambda (source)
                             (if (equal (assoc-default 'name source)
                                        source-name)
                                 source))
                           anything-sources))
             (action (assoc-default 'action source))
             (type (assoc-default 'type source)))

        (unless action
          (unless type
            (error "Source %s has no action and no type defined." source-name))
          (setq action (assoc-default type anything-type-actions)))

        (let* ((transformer (assoc-default type anything-action-transformers)))
          (if transformer
              (funcall transformer action (anything-get-selection))
            action))))))


(defun anything-select-action ()
  "Select an action for the currently selected candidate."
  (interactive)
  (if anything-saved-sources
      (error "Already showing the action list"))

  (setq anything-saved-selection (anything-get-selection))
  (unless anything-saved-selection
    (error "Nothing is selected."))

  (let ((actions (anything-get-action)))
    (if (not (anything-list-but-not-lambda-p actions))
        (message "There is only one possible action.")

      (setq anything-saved-sources anything-sources)
      (setq anything-sources `(((name . "Actions")
                                (candidates . ,actions))))
      (delete-minibuffer-contents)
      (setq anything-pattern 'dummy)    ; so that it differs from the
                                        ; previous one
      (anything-check-minibuffer-input))))


(defun anything-initialize ()
  "Initialize anything settings and set up the anything buffer."
  ;; Call the init-func for sources where appropriate
  (dolist (source anything-sources)
    (let ((init-func (cdr (assoc 'init-func source))))
      (if init-func
          (funcall init-func))))

  (setq anything-pattern "")
  (setq anything-candidate-cache nil)
  (setq anything-saved-sources nil)

  (with-current-buffer (get-buffer-create anything-buffer)
    (setq cursor-type nil)
    (setq mode-name "Anything"))

  (if anything-selection-overlay
      ;; make sure the overlay belongs to the anything buffer if
      ;; it's newly created
      (move-overlay anything-selection-overlay (point-min) (point-min)
                    (get-buffer anything-buffer))

    (setq anything-selection-overlay 
          (make-overlay (point-min) (point-min) (get-buffer anything-buffer)))
    (overlay-put anything-selection-overlay 'face 'highlight))

  (if anything-enable-digit-shortcuts
      (unless anything-digit-overlays
        (dotimes (i 9)
          (push (make-overlay (point-min) (point-min)
                              (get-buffer anything-buffer))
                anything-digit-overlays)
          (overlay-put (car anything-digit-overlays)
                       'before-string (concat (int-to-string (1+ i)) " - ")))
        (setq anything-digit-overlays (nreverse anything-digit-overlays)))

    (when anything-digit-overlays
      (dolist (overlay anything-digit-overlays)
        (delete-overlay overlay))
      (setq anything-digit-overlays nil))))


(defun anything-cleanup ()
  "Clean up the mess."
  (if anything-saved-sources
      (setq anything-sources anything-saved-sources))
  (with-current-buffer anything-buffer
    (setq cursor-type t))
  (anything-kill-async-processes))


(defun anything-previous-line ()
  "Move selection to the previous line."
  (interactive)
  (anything-move-selection 'line 'previous))


(defun anything-next-line ()
  "Move selection to the next line."
  (interactive)
  (anything-move-selection 'line 'next))


(defun anything-previous-page ()
  "Move selection back with a pageful."
  (interactive)
  (anything-move-selection 'page 'previous))


(defun anything-next-page ()
  "Move selection forward with a pageful."
  (interactive)
  (anything-move-selection 'page 'next))


(defun anything-previous-source ()
  "Move selection to the previous source."
  (interactive)
  (anything-move-selection 'source 'previous))


(defun anything-next-source ()
  "Move selection to the next source."
  (interactive)
  (anything-move-selection 'source 'next))


(defun anything-move-selection (unit direction)
  "Move the selection marker to a new position determined by
UNIT and DIRECTION."
  (unless (= (buffer-size (get-buffer anything-buffer)) 0)
    (save-selected-window
      (select-window (get-buffer-window anything-buffer))

      (case unit
        (line (forward-line (case direction
                              (next 1)
                              (previous -1)
                              (t (error "Invalid direction.")))))

        (page (case direction
                (next (condition-case nil
                          (scroll-up)
                        (end-of-buffer (goto-char (point-max)))))
                (previous (condition-case nil
                              (scroll-down)
                            (beginning-of-buffer (goto-char (point-min)))))
                (t (error "Invalid direction."))))

        (source (case direction
                   (next (goto-char (or (anything-get-next-header-pos)
                                        (point-min))))
                   (previous (progn
                               (forward-line -1)
                               (if (bobp)
                                   (goto-char (point-max))
                                 (if (anything-pos-header-line-p)
                                     (forward-line -1)
                                   (forward-line 1)))
                               (goto-char (anything-get-previous-header-pos))
                               (forward-line 1)))
                   (t (error "Invalid direction."))))

        (t (error "Invalid unit.")))

      (while (anything-pos-header-line-p)
        (forward-line (if (and (eq direction 'previous)
                               (not (eq (line-beginning-position)
                                        (point-min))))
                          -1
                        1)))

      (if (eobp)
          (forward-line -1))

      (anything-mark-current-line))))


(defun anything-mark-current-line ()
  "Move selection overlay to current line."
  (move-overlay anything-selection-overlay
                (line-beginning-position)
                (1+ (line-end-position))))


(defun anything-select-with-digit-shortcut ()
  (interactive)
  (if anything-enable-digit-shortcuts
      (let* ((index (- (event-basic-type (elt (this-command-keys-vector) 0)) ?1))
             (overlay (nth index anything-digit-overlays)))
        (if (overlay-buffer overlay)
            (save-selected-window
              (select-window (get-buffer-window anything-buffer))          
              (goto-char (overlay-start overlay))
              (anything-mark-current-line)
              (anything-exit-minibuffer))))))


(defun anything-exit-minibuffer ()
  "Select the current candidate by exiting the minibuffer."
  (interactive)
  (setq anything-iswitchb-candidate-selected t)
  (exit-minibuffer))


(defun anything-get-next-header-pos ()
  "Return the position of the next header from point."
  (next-single-property-change (point) 'anything-header))


(defun anything-get-previous-header-pos ()
  "Return the position of the previous header from point"
  (previous-single-property-change (point) 'anything-header))


(defun anything-pos-header-line-p ()
  "Return t if the current line is a header line."
  (or (get-text-property (line-beginning-position) 'anything-header)
      (get-text-property (line-beginning-position) 'anything-header-separator)))


(defun anything-get-candidates (source)
  "Retrieve and return the list of candidates from
SOURCE."
  (let* ((candidate-source (cdr (assoc 'candidates source)))
         (candidates
          (if (functionp candidate-source)
              (funcall candidate-source)
            (if (listp candidate-source)
                candidate-source
              (if (and (symbolp candidate-source)
                       (boundp candidate-source))
                  (symbol-value candidate-source)
                (error (concat "Candidates must either be a function, "
                               " a variable or a list: %s")
                       candidate-source))))))
    (if (processp candidates)
        candidates
      (anything-transform-candidates candidates source))))
         

(defun anything-transform-candidates (candidates source)
  "Transform CANDIDATES according to `anything-candidate-transformers'."
  (let* ((type (cdr (assoc 'type source)))
         (transformer (cdr (assoc type anything-candidate-transformers))))
    (if transformer
        (funcall transformer candidates)
      candidates)))


(defun anything-get-cached-candidates (source)
  "Return the cached value of candidates for SOURCE.
Cache the candidates if there is not yet a cached value."
  (let* ((name (cdr (assoc 'name source)))
         (candidate-cache (assoc name anything-candidate-cache))
         candidates)

    (if candidate-cache
        (setq candidates (cdr candidate-cache))

      (setq candidates (anything-get-candidates source))

      (if (processp candidates)
          (progn
            (push (cons candidates
                        (append source 
                                (list (cons 'item-count 0)
                                      (cons 'incomplete-line ""))))
                  anything-async-processes)
            (set-process-filter candidates 'anything-output-filter)
            (setq candidates nil))

        (setq candidate-cache (cons name candidates))
        (push candidate-cache anything-candidate-cache)))

    candidates))


(defun anything-output-filter (process string)
  "Process output from locate."
  (let* ((process-assoc (assoc process anything-async-processes))
         (process-info (cdr process-assoc))
         (insertion-marker (cdr (assoc 'insertion-marker process-info)))
         (incomplete-line-info (assoc 'incomplete-line process-info))
         (item-count-info (assoc 'item-count process-info)))

    (with-current-buffer anything-buffer
      (save-excursion
        (if insertion-marker
            (goto-char insertion-marker)
        
          (goto-char (point-max))
          (anything-insert-header (cdr (assoc 'name process-info)))
          (setcdr process-assoc
                  (append process-info `((insertion-marker . ,(point-marker))))))

        (insert-before-markers (cdr incomplete-line-info))

        (let ((lines (split-string string "\n"))
              candidates)
          (while lines
            (if (not (cdr lines))
                ;; store last incomplete line until new output arrives
                (setcdr incomplete-line-info (car lines))

              (push (car lines) candidates)
              (incf (cdr item-count-info))
              (when (>= (cdr item-count-info) anything-candidate-number-limit)
                (setq lines nil)
                (anything-kill-async-process process)))
                  
            (pop lines))

          (setq candidates (reverse candidates))
          (dolist (candidate (anything-transform-candidates candidates process-info))
            (anything-insert-match candidate 'insert-before-markers))))

      (run-hooks 'anything-update-hook)

      (if (bobp)
          (anything-next-line)

        (save-selected-window
          (select-window (get-buffer-window anything-buffer))
          (anything-mark-current-line))))))


(defun anything-kill-async-processes ()
  "Kill all known asynchronous processes according to
`anything-async-processes'."
    "Kill locate process."
    (dolist (process-info anything-async-processes)
      (anything-kill-async-process (car process-info)))
    (setq anything-async-processes nil))


(defun anything-kill-async-process (process)
  "Kill PROCESS and detach the associated functions."
  (set-process-filter process nil)
  (delete-process process))
  

(defun anything-transform-files (files)
  "Transform file candidates."
  (let ((boring-file-regexp 
         (concat "\\(?:" (regexp-opt completion-ignored-extensions) "\\)\\'")))
    (mapcar (lambda (file)
              ;; Add shadow face property to boring files.
              (if (string-match boring-file-regexp file)
                  (setq file (propertize file 'face 'file-name-shadow)))

              ;; replace path of HOME directory in paths with the string
              ;; <home>
              (let ((home (replace-regexp-in-string "\\\\" "/" ; stupid Windows...
                                                    (getenv "HOME"))))
                (if (string-match home file)
                    (cons (replace-match "<home>" nil nil file) file)
                  file)))
            files)))


(defun anything-insert-header (name)
  "Insert header of source NAME into the anything buffer."
  (unless (bobp)
    (let ((start (point)))
      (insert "\n")
      (put-text-property start (point) 'anything-header-separator t)))

  (let ((start (point)))
    (insert name)
    (put-text-property (line-beginning-position)
                       (line-end-position) 'anything-header t)
    (insert "\n")
    (put-text-property start (point) 'face anything-header-face)))


;;----------------------------------------------------------------------
;; Iswitchb integration
;;----------------------------------------------------------------------

(defvar anything-iswitchb-idle-delay 1
  "Show anything completions if the user is idle that many
  seconds after typing.")

(defvar anything-iswitchb-dont-touch-iswithcb-keys nil
  "If t then those commands are not bound from `anything-map'
  under iswitchb which would override standard iswithcb keys.

This allows an even more seamless integration with iswitchb for
those who prefer using iswitchb bindings even if the anything
completions buffer is popped up.

Note that you can bind alternative keys for the same command in
`anything-map', so that you can use different keys for anything
under iswitchb. For example, I bind the character \ to
`anything-exit-minibuffer' which key is just above Enter on my
keyboard. This way I can switch buffers with Enter and choose
anything completions with \.")

;;----------------------------------------------------------------------

(defvar anything-iswitchb-candidate-selected nil
  "Indicates whether an anything candidate is selected from iswitchb.")

(defvar anything-iswitchb-window-configuration nil
  "Saved configuration of the windows, before the anything buffer
  was popped up.")

(defvar anything-iswitchb-saved-keys nil
  "The original in iswitchb before binding anything keys.")


(defun anything-iswitchb-setup ()
  "Integrate anything completion into iswitchb.

If the user is idle for `anything-iswitchb-idle-delay' seconds
after typing something into iswitchb then anything candidates are
shown for the current iswitchb input.

ESC cancels anything completion and returns to normal iswitchb."
  (interactive)
  ;; disable timid completion during iswitchb
  (put 'iswitchb-buffer 'timid-completion 'disabled)
  (add-hook 'minibuffer-setup-hook  'anything-iswitchb-minibuffer-setup)

  (defadvice iswitchb-visit-buffer
    (around anything-iswitchb-visit-buffer activate)
    (if anything-iswitchb-candidate-selected
        (anything-execute-selection-action)
      ad-do-it))

  (defadvice iswitchb-possible-new-buffer
    (around anything-iswitchb-possible-new-buffer activate)
    (if anything-iswitchb-candidate-selected
        (anything-execute-selection-action)
      ad-do-it))

  (message "Iswitchb integration is activated."))


(defun anything-iswitchb-minibuffer-setup ()
  (when (eq this-command 'iswitchb-buffer)
    (add-hook 'minibuffer-exit-hook  'anything-iswitchb-minibuffer-exit)

    (setq anything-iswitchb-window-configuration nil)
    (setq anything-iswitchb-candidate-selected nil)
    (add-hook 'anything-update-hook 'anything-iswitchb-handle-update)

    (anything-initialize)
    
    (add-hook 'post-command-hook 'anything-iswitchb-check-input)))


(defun anything-iswitchb-minibuffer-exit ()
  (remove-hook 'minibuffer-exit-hook  'anything-iswitchb-minibuffer-exit)
  (remove-hook 'post-command-hook 'anything-iswitchb-check-input)
  (remove-hook 'anything-update-hook 'anything-iswitchb-handle-update)

  (anything-cleanup)

  (when anything-iswitchb-window-configuration
    (set-window-configuration
     anything-iswitchb-window-configuration)
    (setq anything-iswitchb-window-configuration nil)))


(defun anything-iswitchb-check-input ()
  "Extract iswitchb input and check if it needs to be handled."
  (if (or anything-iswitchb-window-configuration
          (and (eq this-command 'self-insert-command)
               (sit-for anything-iswitchb-idle-delay)))
      (anything-check-new-input iswitchb-text)))


(defun anything-iswitchb-handle-update ()
  "Pop up the anything buffer if it's not empty and it's not
shown yet and bind anything commands in iswitchb. Alternatively,
hide the anything buffer if it is shown and became empty, and
restore iswitchb keybindings."
  (unless (or (equal (buffer-size (get-buffer anything-buffer)) 0)
              anything-iswitchb-window-configuration)
    (setq anything-iswitchb-window-configuration
          (current-window-configuration))
    (save-selected-window
      (pop-to-buffer anything-buffer))

    (with-current-buffer (window-buffer (active-minibuffer-window))
      (let* ((anything-prefix "anything-")
             (prefix-length (length anything-prefix))
             (commands 
              (delete-dups
               (remove-if 'null
                          (mapcar 
                           (lambda (binding)
                             (let ((command (cdr binding)))
                               (when (and (symbolp command)
                                          (eq (compare-strings 
                                               anything-prefix 
                                               0 prefix-length
                                               (symbol-name command)
                                               0 prefix-length)
                                              t))
                                 command)))
                           (cdr anything-map)))))
             (bindings (mapcar (lambda (command)
                                 (cons command 
                                       (where-is-internal command anything-map)))
                               commands)))

        (push (list 'anything-iswitchb-cancel-anything (kbd "<ESC>"))
              bindings)

        (setq anything-iswitchb-saved-keys nil)

      (let* ((iswitchb-prefix "iswitchb-")
             (prefix-length (length iswitchb-prefix)))
        (dolist (binding bindings)
          (dolist (key (cdr binding))
            (let ((old-command (lookup-key (current-local-map) key)))
              (unless (and anything-iswitchb-dont-touch-iswithcb-keys
                           (symbolp old-command)
                           (eq (compare-strings iswitchb-prefix 
                                                0 prefix-length
                                                (symbol-name old-command)
                                                0 prefix-length)
                               t))
                (push (cons key old-command)
                      anything-iswitchb-saved-keys)
                (define-key (current-local-map) key (car binding)))))))))))


(defun anything-iswitchb-cancel-anything ()
  "Cancel anything completion and return to standard iswitchb."
  (interactive)
  (save-excursion
    (dolist (binding anything-iswitchb-saved-keys)
      (define-key (current-local-map) (car binding) (cdr binding)))
    (anything-iswitchb-minibuffer-exit)))


(defun anything-transform-file-actions (actions candidate)
  "Append a `load-file' action to the list of actions if the
selected file is a .el file."
  (if (and (listp actions)
           (equal (file-name-extension candidate) "el"))
      (append actions '(("Load Emacs Lisp File" . load-file)))
    actions))


(provide 'anything)
;;; anything.el ends here
