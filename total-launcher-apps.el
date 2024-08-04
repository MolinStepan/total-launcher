
;;; Code:

(defvar total-launcher-apps-prefix nil
  "Prefix of apps.")

(defvar total-launcher--apps-cache nil
  "Cache of desktop files data.")

(defvar total-launcher-apps-add-keywords nil
  "If total launcher should add keywords to name of apps.")

(defvar total-launcher--apps-cache-timestamp nil
  "Time when we last updated the cached application list.")

(defvar total-launcher--cached-desktop-files nil
  "List of cached desktop files.")


(defcustom total-launcher-apps-directories
  (mapcar (lambda (dir) (expand-file-name "applications" dir))
	  (cons (xdg-data-home)
		(xdg-data-dirs)))
  "Directories in which to search for applications (.desktop files)."
  :type '(repeat directory))


(defun total-launcher-list-desktop-files ()
  "Return an alist of all Linux applications.
Each list entry is a pair of (desktop-name . desktop-file).
This function always returns its elements in a stable order."
  (let ((hash (make-hash-table :test #'equal))
	result)
    (dolist (dir total-launcher-apps-directories)
      (when (file-exists-p dir)
	(let ((dir (file-name-as-directory dir)))
	  (dolist (file (directory-files-recursively dir ".*\\.desktop$"))
	    (let ((id (subst-char-in-string ?/ ?- (file-relative-name file dir))))
	      (when (and (not (gethash id hash)) (file-readable-p file))
		(push (cons id file) result)
		(puthash id file hash)))))))
    result))

(defun total-launcher-parse-desktop-files (files)
  "Parse the .desktop files to return usable informations."
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (entry files hash)
      (let ((file (cdr entry)))
	(with-temp-buffer
	  (insert-file-contents file)
	  (goto-char (point-min))
	  (let ((start (re-search-forward "^\\[Desktop Entry\\] *$" nil t))
		(end (re-search-forward "^\\[" nil t))
		(visible t)
		name comment exec)
	    (catch 'break
	      (unless start
		(message "Warning: File %s has no [Desktop Entry] group" file)
		(throw 'break nil))

	      (goto-char start)
	      (when (re-search-forward "^\\(Hidden\\|NoDisplay\\) *= *\\(1\\|true\\) *$" end t)
		(setq visible nil))
	      (setq name (match-string 1))

	      (goto-char start)
	      (unless (re-search-forward "^Type *= *Application *$" end t)
		(throw 'break nil))
	      (setq name (match-string 1))

	      (goto-char start)
	      (unless (re-search-forward "^Name *= *\\(.+\\)$" end t)
		(push file counsel-linux-apps-faulty)
		(message "Warning: File %s has no Name" file)
		(throw 'break nil))
	      (setq name (match-string 1))


	      (goto-char start)
	      (when (re-search-forward "^Comment *= *\\(.+\\)$" end t)
		(setq comment (match-string 1)))
	      
	      (setq keywords nil)
	      (if total-launcher-apps-add-keywords
		  (when (re-search-forward "^Keywords *= *\\(.+\\)$" end t)
		    (setq keywords (match-string 1))))

	      (goto-char start)
	      (unless (re-search-forward "^Exec *= *\\(.+\\)$" end t)
		;; Don't warn because this can technically be a valid desktop file.
		(throw 'break nil))
	      (setq exec (match-string 1))

	      (goto-char start)
	      (when (re-search-forward "^TryExec *= *\\(.+\\)$" end t)
		(let ((try-exec (match-string 1)))
		  (unless (locate-file try-exec exec-path nil #'file-executable-p)
		    (throw 'break nil))))

	      (if total-launcher-apps-add-keywords
					; FIXME
		  (puthash (if keywords 
			       (concat 
				total-launcher-apps-prefix 
				name 
				" [" keywords "]") 
			     (concat 
			      total-launcher-apps-prefix 
			      name))
			   (list (cons 'exec exec)
				 (cons 'comment comment)
				 (cons 'visible visible))
			   hash)
		(puthash name
			 (list (cons 'exec exec)
			       (cons 'comment comment)
			       (cons 'visible visible))
			 hash)
		))))))))

(defun total-launcher-list-apps ()
  "Return list of all Linux .desktop applications."
  (let* ((new-desktop-alist (total-launcher-list-desktop-files))
	 (new-files (mapcar 'cdr new-desktop-alist)))
    (unless (and (equal new-files total-launcher--cached-desktop-files)
		 (null (cl-find-if
			(lambda (file)
			  (time-less-p
			   total-launcher--apps-cache-timestamp
			   (nth 5 (file-attributes file))))
			new-files)))
      (setq total-launcher--apps-cache (total-launcher-parse-desktop-files new-desktop-alist))
      (setq total-launcher--apps-cache-timestamp (current-time))
      (setq total-launcher--cached-desktop-files new-files)))
  total-launcher--apps-cache)

(provide 'total-launcher-apps)
;;; total-launcher-apps.el ends here

