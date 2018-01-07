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
;;; Commentary:

;;; Code:

;;
;; Customs
;;

(defcustom dashboard-page-separator "\n\f\n"
  "Separator to use between the different pages."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-image-banner-max-height 0
  "Maximum height of banner image.

This setting applies only if Emacs is compiled with Imagemagick
support.  When value is non-zero the image banner will be resized
to the specified height, with aspect ratio preserved."
  :type 'integer
  :group 'dashboard)

(defcustom dashboard-image-banner-max-width 0
  "Maximum width of banner image.

This setting applies if Emacs is compiled with Imagemagick
support.  When value is non-zero the image banner will be resized
to the specified width, with aspect ratio preserved."
  :type 'integer
  :group 'dashboard)

(defconst dashboard-banners-directory
  (concat (file-name-directory
	   (locate-library "dashboard"))
	   "/banners/"))

(defconst dashboard-banner-official-png
  (expand-file-name (concat dashboard-banners-directory "emacs.png"))
  "Emacs banner image.")

(defconst dashboard-banner-logo-png
  (expand-file-name (concat dashboard-banners-directory "logo.png"))
  "Emacs banner image.")

(defconst dashboard-banner-length 75
  "Width of a banner.")

(defvar dashboard-banner-logo-title "Welcome to Emacs!"
   "Specify the startup banner.")

(defvar dashboard-startup-banner 'official
   "Specify the startup banner.
Default value is `official', it displays
the Emacs logo.  `logo' displays Emacs alternative logo.
An integer value is the index of text
banner.  A string value must be a path to a .PNG file.
If the value is nil then no banner is displayed.")

(defvar dashboard-buffer-last-width nil
  "Previous width of dashboard-buffer.")

(defvar dashboard-item-generators  '((recents   . dashboard-insert-recents)
                                     (bookmarks . dashboard-insert-bookmarks)
                                     (projects  . dashboard-insert-projects)
                                     (agenda    . dashboard-insert-agenda)
                                     (registers . dashboard-insert-registers)))

(defvar dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (agenda    . 5))
  "Association list of items to show in the startup buffer.
Will be of the form `(list-type . list-size)`.
If nil it is disabled.  Possible values for list-type are:
`recents' `bookmarks' `projects' `agenda' `registers'")

(defvar dashboard-items-default-length 20
  "Length used for startup lists with otherwise unspecified bounds.
Set to nil for unbounded.")

;;
;; Faces
;;
(defface dashboard-banner-logo-title-face
  '((t :inherit default))
  "Face used for the banner title.")

(defface dashboard-heading-face
  '((t :inherit default))
  "Face used for widget headings.")

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

(defmacro dashboard-insert-shortcut (shortcut-char
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

(defun dashboard-insert-heading (heading)
  "Insert a widget heading in dashboard buffer."
  (insert (propertize heading 'face 'dashboard-heading-face)))

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

(defun dashboard-insert-image-banner (banner)
  "Display an image BANNER."
  (when (file-exists-p banner)
    (let* ((title dashboard-banner-logo-title)
           (spec (if (image-type-available-p 'imagemagick)
                     (apply 'create-image banner 'imagemagick nil
                            (append (when (> dashboard-image-banner-max-width 0)
                                      (list :max-width dashboard-image-banner-max-width))
                                    (when (> dashboard-image-banner-max-height 0)
                                      (list :max-height dashboard-image-banner-max-height))))
                   (create-image banner)))
           (size (image-size spec))
           (width (car size))
           (left-margin (max 0 (floor (- dashboard-banner-length width) 2))))
      (goto-char (point-min))
      (insert "\n")
      (insert (make-string left-margin ?\ ))
      (insert-image spec)
      (insert "\n\n")
      (when title
	(insert (make-string (max 0 (floor (/ (- dashboard-banner-length
						 (+ (length title) 1)) 2))) ?\ ))
	(insert (format "%s\n\n" (propertize title 'face 'dashboard-banner-logo-title-face)))))))

(defun dashboard-get-banner-path (index)
  "Return the full path to banner with index INDEX."
  (concat dashboard-banners-directory (format "%d.txt" index)))

(defun dashboard-choose-banner ()
  "Return the full path of a banner based on the dotfile value."
  (when dashboard-startup-banner
    (cond ((eq 'official dashboard-startup-banner)
           (if (and (display-graphic-p) (image-type-available-p 'png))
               dashboard-banner-official-png
             (dashboard-get-banner-path 1)))
	  ((eq 'logo dashboard-startup-banner)
           (if (and (display-graphic-p) (image-type-available-p 'png))
               dashboard-banner-logo-png
             (dashboard-get-banner-path 1)))
          ((integerp dashboard-startup-banner)
           (dashboard-get-banner-path dashboard-startup-banner))
          ((and dashboard-startup-banner
                (image-type-available-p (intern (file-name-extension
                                                 dashboard-startup-banner)))
                (display-graphic-p))
           (if (file-exists-p dashboard-startup-banner)
               dashboard-startup-banner
	     (message (format "could not find banner %s"
			      dashboard-startup-banner))
	     (dashboard-get-banner-path 1)))
          (t (dashboard-get-banner-path 1)))))

(defun dashboard-insert-banner ()
  "Insert Banner at the top of the dashboard."
  (goto-char (point-max))
  (let ((banner (dashboard-choose-banner))
        (buffer-read-only nil))
    (progn
      (when banner
	(if (image-type-available-p (intern (file-name-extension banner)))
            (dashboard-insert-image-banner banner)
          (dashboard-insert-ascii-banner-centered banner))))))

;;
;; Recentf
;;
(defun dashboard-insert-recentf-list (list-display-name list)
  "Render LIST-DISPLAY-NAME title and items of LIST."
  (when (car list)
    (dashboard-insert-heading list-display-name)
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
    (dashboard-insert-shortcut "r" "Recent Files:")))


;;
;; Bookmarks
;;
(defun dashboard-insert-bookmark-list (list-display-name list)
  "Render LIST-DISPLAY-NAME title and bookmarks items of LIST."
  (when (car list)
    (dashboard-insert-heading list-display-name)
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
    (dashboard-insert-shortcut "m" "Bookmarks:")))

;;
;; Projectile
;;
(defun dashboard-insert-project-list (list-display-name list)
  "Render LIST-DISPLAY-NAME title and project items of LIST."
  (when (car list)
    (dashboard-insert-heading list-display-name)
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
  ;; For some reason, projectile has to be loaded here
  ;; before trying to load projects list
  (projectile-mode)
  (if (bound-and-true-p projectile-mode)
      (progn
	(projectile-load-known-projects)
	(when (dashboard-insert-project-list
	       "Projects:"
	       (dashboard-subseq (projectile-relevant-known-projects)
				 0 list-size))
	  (dashboard-insert-shortcut "p" "Projects:")))
    (message "Failed to load projects list")))

;;
;; Org Agenda
;;
(defun dashboard-insert-agenda-list (list-display-name list)
  "Render LIST-DISPLAY-NAME title and agenda items from LIST."
  (dashboard-insert-heading list-display-name)
  (if (car list)
      (mapc (lambda (el)
              (insert "\n    ")
              (let ((filename (nth 4 el))
                    (lineno (nth 3 el))
                    (title (nth 0 el)))
                (widget-create 'push-button
                               :action `(lambda (&rest ignore)
                                          (let ((buffer (find-file-other-window ,filename)))
                                            (with-current-buffer buffer
                                              (goto-char ,lineno)
                                              )
                                            (switch-to-buffer buffer)))
                               :mouse-face 'highlight
                               :follow-link "\C-m"
                               :button-prefix ""
                               :button-suffix ""
                               :format "%[%t%]"
                               (format "%s" title)))
              )
            list)
    (insert (propertize "\n    --- No items ---" 'face 'widget-button))))

(defun dashboard-timestamp-to-gregorian-date (timestamp)
  "Convert TIMESTAMP to a gregorian date.

The result can be used with functions like
`calendar-date-compare'."
  (let ((decoded-timestamp (decode-time timestamp)))
    (list (nth 4 decoded-timestamp)
          (nth 3 decoded-timestamp)
          (nth 5 decoded-timestamp))))

(defun dashboard-date-due-p (timestamp &optional due-date)
  "Check if TIMESTAMP is today or in the past.

If DUE-DATE is nil, compare TIMESTAMP to today; otherwise,
compare to the date in DUE-DATE.

The time part of both TIMESTAMP and DUE-DATE is ignored, only the
date part is considered."
  (unless due-date
    (setq due-date (current-time)))
  (setq due-date (time-add due-date 86400))
  (let* ((gregorian-date (dashboard-timestamp-to-gregorian-date timestamp))
         (gregorian-due-date (dashboard-timestamp-to-gregorian-date due-date)))
    (calendar-date-compare (list gregorian-date)
                           (list gregorian-due-date))))

(defun dashboard-get-agenda ()
  "Get agenda items for today."
  (org-compile-prefix-format 'agenda)
  (let* ((filtered-entries nil))
    (org-map-entries
     (lambda ()
       (let* ((schedule-time (org-get-scheduled-time (point)))
             (deadline-time (org-get-deadline-time (point)))
             (item (org-agenda-format-item
		    (format-time-string "%Y-%m-%d" deadline-time)
                    (org-get-heading t t)
                    (org-outline-level)
                    (org-get-category)
                    (org-get-tags)
                    t))
             (loc (point))
             (file (buffer-file-name)))
         (when (and (not (org-entry-is-done-p))
                    (or (and schedule-time (dashboard-date-due-p schedule-time))
                        (and deadline-time (dashboard-date-due-p deadline-time))))
           (setq filtered-entries
                 (append filtered-entries
                         (list (list item schedule-time deadline-time loc file)))))))
     nil
     'agenda)
    filtered-entries))

(defun dashboard-insert-agenda (list-size)
  "Add the list of LIST-SIZE items of agenda."
  (when (dashboard-insert-agenda-list "Agenda for today:"
                                      (dashboard-get-agenda))
    (dashboard-insert-shortcut "a" "Agenda for today:")))

;;
;; Registers
;;
(defun dashboard-insert-register-list (list-display-name list)
  "Render LIST-DISPLAY-NAME title and registers items of LIST."
  (when (car list)
    (dashboard-insert-heading list-display-name)
    (mapc (lambda (el)
            (let ((register (car el)))
              (insert "\n    ")
              (widget-create 'push-button
                             :action `(lambda (&rest ignore) (jump-to-register ,register))
                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""
                             :format "%[%t%]"
                             (format "%c - %s" register (register-describe-oneline register)))))
          list)))

(defun dashboard-insert-registers (list-size)
  "Add the list of LIST-SIZE items of registers."
  (require 'register)
  (when (dashboard-insert-register-list
         "Registers:"
         (dashboard-subseq register-alist 0 list-size))
    (dashboard-insert-shortcut "e" "Registers:")))


;; Forward declartions for optional dependency to keep check-declare happy.
(declare-function bookmark-get-filename "ext:bookmark.el")
(declare-function bookmark-all-names "ext:bookmark.el")
(declare-function projectile-load-known-projects "ext:projectile.el")
(declare-function projectile-relevant-known-projects "ext:projectile.el")

(provide 'dashboard-widgets)

;;; dashboard-widgets.el ends here
