;;; Code:

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

(provide 'total-launcher-recentf)
;;; total-launcher-recentf.el ends here
