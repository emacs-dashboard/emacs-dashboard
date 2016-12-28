;;; dashboard.el --- A startup screen extracted from Spacemacs

;; Copyright (c) 2016 Rakan Al-Hneiti & Contributors
;;
;; Author: Rakan Al-Hneiti
;; URL: https://github.com/rakanalh/emacs-dashboard
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Created: October 05, 2016
;; Modified: October 06, 2016
;; Version: 1.0.3
;; Keywords: startup screen tools
;; Package-Requires: ((emacs "24.4") (page-break-lines "0.11"))
;;; Commentary:

;; A shameless extraction of Spacemacs’ startup screen, with sections for
;; bookmarks, projectil projects and more.

;;; Code:

;;
;; Customs
;;
(defcustom dashboard-page-separator "\n\f\n"
  "Separator to use between the different pages."
  :type 'string
  :group 'dashboard)

(defconst dashboard-banner-length 75
	  "Width of a banner.")
(defvar dashboard-buffer-last-width nil
  "Previous width of dashboard-buffer.")

(defvar dashboard-item-generators  '((recents   . dashboard-insert-recents)
                                     (bookmarks . dashboard-insert-bookmarks)
                                     (projects  . dashboard-insert-projects)))

(defvar dashboard-items '((recents   . 5)
			  (bookmarks . 5))
  "Association list of items to show in the startup buffer.
Will be of the form `(list-type . list-size)`.
If nil it is disabled.  Possible values for list-type are:
`recents' `bookmarks' `projects'")

(defvar dashboard-items-default-length 20
  "Length used for startup lists with otherwise unspecified bounds.
Set to nil for unbounded.")

;;
;; Generic widget helpers
;;
(defun dashboard-subseq (seq start end)
  "Return the subsequence of SEQ from START to END..
Uses `cl-subseq`, but accounts for end points greater than the size of the
list.
Return entire list if `END' is omitted."
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end)
                              (min len end)))))

(defmacro dashboard-insert--shortcut (shortcut-char
				      search-label
				      &optional no-next-line)
  "Insert a shortcut SHORTCUT-CHAR for a given SEARCH-LABEL.
Optionally, provide NO-NEXT-LINE to move the cursor forward a line."
  `(define-key dashboard-mode-map ,shortcut-char (lambda ()
			       (interactive)
			       (unless (search-forward ,search-label (point-max) t)
				 (search-backward ,search-label (point-min) t))
			       ,@(unless no-next-line
				   '((forward-line 1)))
			       (back-to-indentation))))

(defun dashboard-append (msg &optional messagebuf)
  "Append MSG to dashboard buffer.
If MESSAGEBUF is not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create "*dashboard*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg))))

(defun dashboard-insert-page-break ()
  "Insert a page break line in dashboard buffer."
  (dashboard-append dashboard-page-separator))

;;
;; BANNER
;;
(defun dashboard-insert-ascii-banner-centered (file)
  "Insert banner from FILE."
  (insert
   (with-temp-buffer
     (insert-file-contents file)
     (let ((banner-width 0))
       (while (not (eobp))
         (let ((line-length (- (line-end-position) (line-beginning-position))))
           (if (< banner-width line-length)
               (setq banner-width line-length)))
         (forward-line 1))
       (goto-char 0)
       (let ((margin (max 0 (floor (/ (- dashboard-banner-length banner-width) 2)))))
         (while (not (eobp))
	   (insert (make-string margin ?\ ))
           (forward-line 1))))
     (buffer-string))))

(defun dashboard-insert-banner ()
  "Insert Banner at the top of the dashboard."
  (goto-char (point-max))
  (dashboard-insert-ascii-banner-centered
   (concat (file-name-directory
	    (locate-library "dashboard"))
	   "banner.txt")))

;;
;; Recentf
;;
(defun dashboard-insert-recentf-list (list-display-name list)
  "Render LIST-DISPLAY-NAME title and items of LIST."
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (find-file-existing ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el)))
          list)))

(defun dashboard-insert-recents (list-size)
  "Add the list of LIST-SIZE items from recently edited files."
  (recentf-mode)
  (when (dashboard-insert-recentf-list
	 "Recent Files:"
	 (dashboard-subseq recentf-list 0 list-size))
    (dashboard-insert--shortcut "r" "Recent Files:")))


;;
;; Bookmarks
;;
(defun dashboard-insert-bookmark-list (list-display-name list)
  "Render LIST-DISPLAY-NAME title and bookmarks items of LIST."
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (bookmark-jump ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (format "%s - %s" el (abbreviate-file-name
                                                 (bookmark-get-filename el)))))
          list)))

(defun dashboard-insert-bookmarks (list-size)
  "Add the list of LIST-SIZE items of bookmarks."
  (require 'bookmark)
  (when (dashboard-insert-bookmark-list
	 "Bookmarks:"
	 (dashboard-subseq (bookmark-all-names)
			   0 list-size))
    (dashboard-insert--shortcut "m" "Bookmarks:")))

;;
;; Projectile
;;
(defun dashboard-insert-project-list (list-display-name list)
  "Render LIST-DISPLAY-NAME title and project items of LIST."
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore)
				      (projectile-switch-project-by-name ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el)))
          list)))

(defun dashboard-insert-projects (list-size)
  "Add the list of LIST-SIZE items of projects."
  (if (bound-and-true-p projectile-mode)
      (progn
	(projectile-load-known-projects)
	(when (dashboard-insert-project-list
	       "Projects:"
	       (dashboard-subseq (projectile-relevant-known-projects)
				 0 list-size))
	  (dashboard-insert--shortcut "p" "Projects:")))
    (error "Projects list depends on 'projectile-mode` to be activated")))

;; Forward declartions for optional dependency to keep check-declare happy.
(declare-function bookmark-get-filename "ext:bookmark.el")
(declare-function bookmark-all-names "ext:bookmark.el")
(declare-function projectile-load-known-projects "ext:projectile.el")
(declare-function projectile-relevant-known-projects "ext:projectile.el")

(provide 'dashboard-widgets)
;;; widgets.el ends here
