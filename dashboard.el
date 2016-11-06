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
;; Version: 1.0.0
;; Keywords: startup screen tools
;; Package-Requires: ((emacs "24.4") (page-break-lines "0.11") (projectile "0.14.0"))
;;; Commentary:

;; A shameless extraction of Spacemacs’ startup screen, with sections for
;; bookmarks, projectile projects and more.

;;; Code:

(eval-when-compile (require 'bookmark))
(eval-when-compile (require 'page-break-lines))

(defun dashboard-subseq (seq start end)
  "Use `cl-subseq`, but accounting for end points greater than the size of the
list. Return entire list if `END' is omitted."
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end)
                              (min len end)))))


;; Custom splash screen
(defvar dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [down-mouse-1] 'widget-button-click)
    map)
  "Keymap for dashboard mode.")

(define-derived-mode dashboard-mode special-mode "Dashboard"
  "Dashboard major mode for startup screen.
\\<dashboard-mode-map>
"
  :group 'dashboard
  :syntax-table nil
  :abbrev-table nil
  (linum-mode -1)
  (setq buffer-read-only t
        truncate-lines t))

(defconst dashboard-banner-length 75
	  "Width of a banner.")

(defun dashboard-insert-ascii-banner-centered (file)
  "Insert banner from FILE."
  (insert-string
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

(defun dashboard-insert-file-list (list-display-name list)
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

(defun dashboard-insert-project-list (list-display-name list)
  "Render LIST-DISPLAY-NAME title and project items of LIST."
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (projectile-switch-project-by-name ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el)))
          list)))

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

(defun dashboard-insert-page-break ()
  "Insert a page break line in dashboard buffer."
  (dashboard-append dashboard-page-separator))

(defun dashboard-append (msg &optional messagebuf)
  "Append MSG to dashboard buffer.
If MESSAGEBUF is not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create "*dashboard*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg))))

(defmacro dashboard-insert--shortcut (shortcut-char search-label &optional no-next-line)
  `(define-key dashboard-mode-map ,shortcut-char (lambda ()
			       (interactive)
			       (unless (search-forward ,search-label (point-max) t)
				 (search-backward ,search-label (point-min) t))
			       ,@(unless no-next-line
				   '((forward-line 1)))
			       (back-to-indentation))))


(defun dashboard-goto-link-line ()
  "Move the point to the beginning of the link line."
  (interactive)
  (with-current-buffer "*dashboard*"
    (goto-char (point-min))
    (re-search-forward "Homepage")
    (beginning-of-line)
    (widget-forward 1)))

(defun dashboard-insert-recents ()
  (recentf-mode)
  (when (dashboard-insert-file-list
	 "Recent Files:"
	 (dashboard-subseq recentf-list 0 list-size))
    (dashboard-insert--shortcut "r" "Recent Files:")))

(defun dashboard-insert-bookmarks ()
  (require 'bookmark)
  (when (dashboard-insert-bookmark-list
	 "Bookmarks:"
	 (dashboard-subseq (bookmark-all-names)
			   0 list-size))
    (dashboard-insert--shortcut "m" "Bookmarks:")))

(defun dashboard-insert-projects ()
  (require 'projectile)
  (projectile-mode)
  (when (dashboard-insert-file-list
	 "Projects:"
	 (dashboard-subseq (projectile-relevant-known-projects)
			   0 list-size))
    (dashboard-insert--shortcut "p" "Projects:")))

(defvar dashboard-item-generators  '((recents   . dashboard-insert-recents)
                                     (bookmarks . dashboard-insert-bookmarks)
                                     (projects  . dashboard-insert-projects)))
(defvar dashboard-items '((recents   . 5)
			  (bookmarks . 5)
			  (projects  . 7))
  "Association list of items to show in the startup buffer of the form
`(list-type . list-size)`. If nil it is disabled.
Possible values for list-type are:
`recents' `bookmarks' `projects'
")
(defvar dashboard-items-default-length 20
  "Length used for startup lists with otherwise unspecified bounds.
Set to nil for unbounded.")

(defun dashboard-insert-startupify-lists ()
  "Insert the list of widgets into the buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*dashboard*")
    (let ((buffer-read-only nil)
          (list-separator "\n\n"))
      (dashboard-insert-banner)
      (dashboard-insert-page-break)
      (mapc (lambda (els)
	      (let* ((el (or (car-safe els) els))
		     (list-size
		      (or (cdr-safe els)
			  dashboard-items-default-length))
		     (item-generator
		      (cdr-safe (assoc el dashboard-item-generators))
		      ))
		(funcall item-generator)
		(dashboard-insert-page-break)
		))
	    dashboard-items))
    (dashboard-mode)))

;;;###autoload
(defun dashboard-setup-startup-hook ()
  "Add post init processing."
  (setq inhibit-startup-screen t)
  (add-hook
   'emacs-startup-hook
   (lambda ()
     ;; Display useful lists of items
     (dashboard-insert-startupify-lists)
     (page-break-lines-mode 1)
     (goto-char (point-min)))
   (redisplay))
  (add-hook 'after-init-hook '(lambda () (switch-to-buffer "*dashboard*"))))

(defgroup dashboard nil
  "Settings that are used in the Dashboard"
  :group 'dashboard)

(defcustom dashboard-page-separator "\n\f\n"
  "Separator to use between the different pages"
  :type 'string
  :group 'dashboard)


(provide 'dashboard)
;;; dashboard.el ends here
