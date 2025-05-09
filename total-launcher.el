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
(defun total-launcher-run-app (prompt sources &optional arg)
  "Launch an application installed on your machine.
When ARG is non-nil, ignore NoDisplay property in *.desktop files."
  (interactive)
  (let* ((candidates (apply 'map-merge 'hash-table sources))
	 (result (completing-read
		  prompt
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
		(puthash (concat total-launcher-apps-prefix name)
			 (list (cons 'exec exec)
			       (cons 'comment comment)
			       (cons 'visible visible))
			 hash)))))))))

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

(defun total-launcher-list-directory-contents-recursively-unfinished (directory hide-path all prefix command-to-open)
  "Return files in DIRECTORY, rercursively.
When HIDE-PATH is non-nil, will only show names of files.
When ALL is non-nil, will search in hidden files also.
This may cause problems if directory contains files with same names.
PREFIX is a prefix to all names.  COMMAND-TO-OPEN is a command to open
files.  Does not work properly with symlinks, directory names including
newline may cause problems."
  (let* ((output (shell-command-to-string
		   (concat (if all "ls -R1pa " "ls -R1p ") directory)))
	 (path-length (length (expand-file-name directory)))
	 (list-to-parse (split-string output "\n"))
	 (hash (make-hash-table :test #'equal))
         (current-line (car list-to-parse))
         (current-directory nil))
         (while list-to-parse
           (setq current-directory (car list-to-parse)
                 list-to-parse (cdr list-to-parse))
           (while (string<> current-line "")
		   ))))



(defun total-launcher-list-directory-contents-recursively (directory hide-path all prefix command-to-open)
  "Return files in DIRECTORY, rercursively.
When HIDE-PATH is non-nil, will only show names of files.
When ALL is non-nil, will search in hidden files also.
This may cause problems if directory contains files with same names.
PREFIX is a prefix to all names.  COMMAND-TO-OPEN is a command to open
files.  Does not work properly with symlinks, directory names including
newline may cause problems."
  (let* ((output (substring
		  (shell-command-to-string
		   (concat (if all "ls -R1pa " "ls -R1p ") directory))
		  0 -1)) ;; Optimise!
	 (path-length (length (expand-file-name directory)))
	 (subdirectories-list (split-string output "\n\n"))
	 (hash (make-hash-table :test #'equal)))
    (dolist (directory-with-contents subdirectories-list hash)
      (let* ((separated (split-string directory-with-contents "\n"))
	     (current-directory (substring (car separated) 0 -1))
	     (files (cdr separated)))
	(if hide-path
	    (dolist (file files)
	      (unless (string= (substring file -1 nil) "/")
		(puthash (concat prefix file)
			 (list (cons 'exec
				     (concat
				      command-to-open " '" current-directory "'/'" file "'"))
			       (cons 'visible t)
			       (cons 'comment nil))
			 hash)))
	  (let* ((local-path (substring current-directory path-length nil)))
	    (dolist (file files)
	      (unless (string= (substring file -1 nil) "/")
		(puthash (concat prefix local-path "/" file)
			 (list (cons 'exec
				     (concat
				      command-to-open " '" current-directory "'/'" file "'"))
			       (cons 'visible t)
			       (cons 'comment nil))
			 hash)))))))))

(defun total-launcher-list-directory-contents (directory all prefix command-to-open)
  "Return files in DIRECTORY, rercursively.
When ALL is non-nil, will search in hidden files also.
This may cause problems if directory contains files with same names.
PREFIX is a prefix to all names.  COMMAND-TO-OPEN is a command to open
files.  Does not work properly with symlinks, directory names including
newline may cause problems."
  (let* ((output (substring
		  (shell-command-to-string (concat (if all "ls -1pa " "ls -1p ") directory))
		  0 -1)) ;; Optimise!
	 (files (split-string output "\n"))
	 (directory-right-name (shell-quote-argument (expand-file-name directory)))
	 (hash (make-hash-table :test #'equal)))
    (dolist (file files hash)
      (unless (string= (substring file -1 nil) "/")
	(puthash (concat prefix file)
		 (list (cons 'exec
			     (concat
			      command-to-open " " directory-right-name "'" file "'"))
		       (cons 'visible t)
		       (cons 'comment nil))
		 hash)))))

(defvar total-launcher-command-to-open-file "xdg-open "
  "Command used to open file.")

(defvar total-launcher-recentf-prefix "Recent: "
  "Prefix of recent files.")

(defun total-launcher-list-recentf ()
  "Return hash table of recent file names (from recentf-list) and commands to open them."
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (filename recentf-list hash)
      (puthash (concat total-launcher-recentf-prefix filename)
	       (list (cons 'exec (concat
				  total-launcher-command-to-open-file
				  (shell-quote-argument (expand-file-name filename))))
		     (cons 'comment nil)
		     (cons 'visible t))
	       hash))))

(provide 'total-launcher)
;;; total-launcher.el ends here
