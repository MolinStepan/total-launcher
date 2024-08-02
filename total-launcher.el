;;; total-launcher.el --- Launch applications from Emacs -*- lexical-binding: t -*-

;; Author: Sebastien Waegeneire
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/sebastienwae/total-launcher

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; total-launcher define the `total-launcher-run-app' command which uses
;; Emacs standard completion feature to select an application installed
;; on your machine and launch it.

;;; Acknowledgements:

;; This package uses code from the Counsel package by Oleh Krehel.
;; https://github.com/abo-abo/swiper

(require 'xdg)
(require 'cl-seq)

(setq total-launcher-prompt "-> ")

(defcustom total-launcher-apps-directories
  (mapcar (lambda (dir) (expand-file-name "applications" dir))
	  (cons (xdg-data-home)
		(xdg-data-dirs)))
  "Directories in which to search for applications (.desktop files)."
  :type '(repeat directory))

(defcustom total-launcher--annotation-function #'total-launcher--annotation-function-default
  "Define the function that genereate the annotation for each completion choices."
  :type 'function)

(defcustom total-launcher--action-function #'total-launcher--action-function-default
  "Define the function that is used to run the selected application."
  :type 'function)

(defvar total-launcher--cache nil
  "Cache of desktop files data.")

(defvar total-launcher--cache-timestamp nil
  "Time when we last updated the cached application list.")

(defvar total-launcher--cached-files nil
  "List of cached desktop files.")

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

(defun total-launcher-parse-files (files)
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

	      (puthash name
		       (list (cons 'file file)
			     (cons 'exec exec)
			     (cons 'comment comment)
			     (cons 'visible visible))
		       hash))))))))

(defun total-launcher-list-apps ()
  "Return list of all Linux .desktop applications."
  (let* ((new-desktop-alist (total-launcher-list-desktop-files))
	 (new-files (mapcar 'cdr new-desktop-alist)))
    (unless (and (equal new-files total-launcher--cached-files)
		 (null (cl-find-if
			(lambda (file)
			  (time-less-p
			   total-launcher--cache-timestamp
			   (nth 5 (file-attributes file))))
			new-files)))
      (setq total-launcher--cache (total-launcher-parse-files new-desktop-alist))
      (setq total-launcher--cache-timestamp (current-time))
      (setq total-launcher--cached-files new-files)))
  total-launcher--cache)

(defun total-launcher--annotation-function-default (choice)
  "Default function to annotate the completion choices."
  (let ((str (cdr (assq 'comment (gethash choice total-launcher--cache)))))
    (when str (concat " - " (propertize str 'face 'completions-annotations)))))

(defun total-launcher--action-function-default (selected)
  "Default function used to run the selected application."
  (let* ((exec (cdr (assq 'exec (gethash selected total-launcher--cache))))
	 (command (let (result)
		    (dolist (chunk (split-string exec " ") result)
		      (unless (or (equal chunk "%U")
				  (equal chunk "%F")
				  (equal chunk "%u")
				  (equal chunk "%f"))
			(setq result (concat result chunk " ")))))))
    (call-process-shell-command command nil 0 nil)))

;;;###autoload
(defun total-launcher-run-app (&optional arg)
  "Launch an application installed on your machine.
When ARG is non-nil, ignore NoDisplay property in *.desktop files."
  (interactive)
  (let* ((candidates (total-launcher-list-apps))
	 (result (completing-read
		  total-launcher-prompt
		  (lambda (str pred flag)
		    (if (eq flag 'metadata)
			'(metadata
			  (annotation-function . (lambda (choice)
						   (funcall
						    total-launcher--annotation-function
						    choice))))
		      (complete-with-action flag candidates str pred)))
		  (lambda (x y)
		    (if arg
			t
		      (cdr (assq 'visible y))))
		  t nil 'total-launcher nil nil)))
    (funcall total-launcher--action-function result)))

;; Provide the total-launcher feature
(provide 'total-launcher)
