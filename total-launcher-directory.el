;;; Code:

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

(provide 'total-launcher-directory)
;;; total-launcher-directory.el ends here
