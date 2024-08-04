;;; Code:

(require 'xdg)
(require 'cl-seq)

(defvar total-launcher-prompt "-> "
  "Prompt of total launcher.")

(defcustom total-launcher--annotation-function #'total-launcher--annotation-function-default
  "Define the function that genereate the annotation for each completion choices."
  :type 'function)

(defcustom total-launcher--action-function #'total-launcher--action-function-default
  "Define the function that is used to run the selected application."
  :type 'function)

(defun total-launcher--annotation-function-default (choice choices-table)
  "Default function to annotate the completion choices."
  (let ((str (cdr (assq 'comment (gethash choice choices-table)))))
    (when str (concat " - " (propertize str 'face 'completions-annotations)))))

(defun total-launcher--action-function-default (selected choices-table)
  "Default function used to run the selected application."
  (let* ((exec (cdr (assq 'exec (gethash selected choices-table))))
	 (command (let (result)
		    (dolist (chunk (split-string exec " ") result)
		      (unless (or (equal chunk "%U")
				  (equal chunk "%F")
				  (equal chunk "%u")
				  (equal chunk "%f"))
			(setq result (concat result chunk " ")))))))
    (message command)
    (call-process-shell-command command nil 0 nil)))

;;;###autoload
(defun total-launcher-run-app (sources &optional arg)
  "Launch an application installed on your machine.
When ARG is non-nil, ignore NoDisplay property in *.desktop files."
  (interactive)
  (let* ((candidates (apply 'map-merge 'hash-table sources))
	 (result (completing-read
		  total-launcher-prompt
		  (lambda (str pred flag)
		    (if (eq flag 'metadata)
			'(metadata
			  (annotation-function . (lambda (choice)
						   (funcall
						    total-launcher--annotation-function
						    choice
                                                    candidates))))
		      (complete-with-action flag candidates str pred)))
		  (lambda (x y)
		    (if arg
			t
		      (cdr (assq 'visible y))))
		  t nil 'total-launcher nil nil)))
    (funcall total-launcher--action-function result candidates)))

;; Provide the total-launcher feature
(provide 'total-launcher)
;;; total-launcher.el ends here
