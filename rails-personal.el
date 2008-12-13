(defun rails-name-model (str)
  (interactive)
  (singularize-string str))

(defun rails-name-models (str)
  (interactive)
  (pluralize-string str))

(defun rails-class-elements (str)
  (let* ( (model-name (rails-name-model str))
	  (name-elements (split-string model-name "_")))
    (mapcar (lambda (f) (capitalize f)) name-elements)))

(defun rails-name-class (str)
  (interactive "s model_name: ")
  (rinari-join-string (rails-class-elements str)))

(defun rails-name-humanize (str)
  (rinari-join-string (rails-class-elements str) " "))

(defun insert-rails-name-model (str)
  (interactive "s model_name: ")
  (insert (rails-name-model str)))

(defun insert-rails-name-models (str)
  (interactive "s model_name: ")
  (insert (rails-name-models str)))

(defun insert-rails-class-elements (str)
  (interactive "s model_name: ")
  (insert (rails-class-elements str)))

(defun insert-rails-name-class (str)
  (interactive "s model_name: ")
  (insert (rails-name-class str)))

(defun insert-rails-name-humanize (str)
  (interactive "*s model_name: ")
  (insert (rails-name-humanize str)))
