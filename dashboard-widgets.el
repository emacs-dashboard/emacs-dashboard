;;; dashboard-widgets.el --- A startup screen extracted from Spacemacs  -*- lexical-binding: t -*-

;; Copyright (c) 2016-2020 Rakan Al-Hneiti <rakan.alhneiti@gmail.com>
;; Copyright (c) 2019-2020 Jesús Martínez <jesusmartinez93@gmail.com>
;; Copyright (c) 2020 Shen, Jen-Chieh <jcs090218@gmail.com>
;;
;; Author: Rakan Al-Hneiti
;; URL: https://github.com/emacs-dashboard/emacs-dashboard
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Created: October 05, 2016
;; Package-Version: 1.8.0-SNAPSHOT
;; Keywords: startup, screen, tools, dashboard
;; Package-Requires: ((emacs "25.3") (page-break-lines "0.11"))
;;; Commentary:

;; An extensible Emacs dashboard, with sections for
;; bookmarks, projects (projectile or project.el), org-agenda and more.

;;; Code:

(require 'cl-lib)

;; Compiler pacifier
(declare-function all-the-icons-icon-for-dir "ext:all-the-icons.el")
(declare-function all-the-icons-icon-for-file "ext:all-the-icons.el")
(declare-function bookmark-get-filename "ext:bookmark.el")
(declare-function bookmark-all-names "ext:bookmark.el")
(declare-function calendar-date-compare "ext:calendar.el")
(declare-function projectile-cleanup-known-projects "ext:projectile.el")
(declare-function projectile-load-known-projects "ext:projectile.el")
(declare-function projectile-mode "ext:projectile.el")
(declare-function projectile-relevant-known-projects "ext:projectile.el")
;;; project.el in Emacs 26 does not contain this function
(declare-function project-known-project-roots "ext:project.el" nil t)
(declare-function org-agenda-format-item "ext:org-agenda.el")
(declare-function org-compile-prefix-format "ext:org-agenda.el")
(declare-function org-entry-is-done-p "ext:org.el")
(declare-function org-get-category "ext:org.el")
(declare-function org-get-deadline-time "ext:org.el")
(declare-function org-get-heading "ext:org.el")
(declare-function org-get-scheduled-time "ext:org.el")
(declare-function org-get-tags "ext:org.el")
(declare-function org-map-entries "ext:org.el")
(declare-function org-outline-level "ext:org.el")
;; Org-time-less-p is define in emacs-27 as time-less-p alias
(when (< emacs-major-version 27)
  (defalias 'org-time-less-p 'time-less-p))
(defvar all-the-icons-dir-icon-alist)
(defvar package-activated-list)

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

(defcustom dashboard-set-heading-icons nil
  "When non nil, heading sections will have icons."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-set-file-icons nil
  "When non nil, file lists will have icons."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-set-navigator nil
  "When non nil, a navigator will be displayed under the banner."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-set-init-info t
  "When non nil, init info will be displayed under the banner."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-set-footer t
  "When non nil, a footer will be displayed at the bottom."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-footer-messages
  '("The one true editor, Emacs!"
    "Who the hell uses VIM anyway? Go Evil!"
    "Free as free speech, free as free Beer"
    "Richard Stallman is proud of you"
    "Happy coding!"
    "Vi Vi Vi, the editor of the beast"
    "Welcome to the church of Emacs"
    "While any text editor can save your files,\
 only Emacs can save your soul"
    "I showed you my source code, pls respond")
  "A list of messages, one of which dashboard chooses to display."
  :type 'list
  :group 'dashboard)

(defcustom dashboard-show-shortcuts t
  "Whether to show shortcut keys for each section."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-org-agenda-categories nil
  "Specify the Categories to consider when using agenda in dashboard.
Example:
'(\"Tasks\" \"Habits\")"
  :type 'list
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

(defcustom dashboard-banner-logo-title "Welcome to Emacs!"
  "Specify the startup banner."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-navigator-buttons nil
  "Specify the navigator buttons.
The format is: 'icon title help action face prefix suffix'.

Example:
'((\"☆\" \"Star\" \"Show stars\" (lambda (&rest _) (show-stars)) 'warning \"[\" \"]\"))"
  :type '(repeat (repeat (list string string string function symbol string string)))
  :group 'dashboard)

(defcustom dashboard-init-info
  ;; Check if package.el was loaded and if package loading was enabled
  (if (bound-and-true-p package-alist)
      (format "%d packages loaded in %s"
              (length package-activated-list) (emacs-init-time))
    (if (and (boundp 'straight--profile-cache)
             (hash-table-p straight--profile-cache))
        (format "%d packages loaded in %s"
                (hash-table-size straight--profile-cache) (emacs-init-time))
      (format "Emacs started in %s" (emacs-init-time))))
  "Init info with packages loaded and init time."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-footer
  (let ((list '("The one true editor, Emacs!"
                "Who the hell uses VIM anyway? Go Evil!"
                "Free as free speech, free as free Beer"
                "Richard Stallman is proud of you"
                "Happy coding!"
                "Vi Vi Vi, the editor of the beast"
                "Welcome to the church of Emacs"
                "While any text editor can save your files,\
 only Emacs can save your soul"
                "I showed you my source code, pls respond"
                )))
    (nth (random (1- (1+ (length list)))) list))
  "A footer with some short message."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-footer-icon
  (if (and (display-graphic-p)
           (or (fboundp 'all-the-icons-fileicon)
               (require 'all-the-icons nil 'noerror)))
      (all-the-icons-fileicon "emacs"
                              :height 1.1
                              :v-adjust -0.05
                              :face 'font-lock-keyword-face)
    (propertize ">" 'face 'dashboard-footer))
  "Footer's icon."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-startup-banner 'official
  "Specify the startup banner.
Default value is `official', it displays
the Emacs logo.  `logo' displays Emacs alternative logo.
An integer value is the index of text
banner.  A string value must be a path to a .PNG or .TXT file.
If the value is nil then no banner is displayed."
  :type '(choice (const  :tag "offical"   official)
                 (const  :tag "logo"      logo)
                 (string :tag "a png or txt path"))
  :group 'dashboard)

(defcustom dashboard-buffer-last-width nil
  "Previous width of dashboard-buffer."
  :type  'integer
  :group 'dashboard)

(defcustom dashboard-item-generators
  '((recents   . dashboard-insert-recents)
    (bookmarks . dashboard-insert-bookmarks)
    (projects  . dashboard-insert-projects)
    (agenda    . dashboard-insert-agenda)
    (registers . dashboard-insert-registers))
  "Association list of items to how to generate in the startup buffer.
Will be of the form `(list-type . list-function)'.
Possible values for list-type are: `recents', `bookmarks', `projects',
`agenda' ,`registers'."
  :type  '(repeat (alist :key-type symbol :value-type function))
  :group 'dashboard)

(defcustom dashboard-projects-backend 'projectile
  "The package that supplies the list of recent projects.
With the value `projectile', the projects widget uses
projectile (available from MELPA), with `project-el' the widget
uses project.el (built-in since Emacs 27.1).

To activate the projects widget, add e.g. `(projects . 10)' to
`dashboard-items' after making sure either of the above packages
is installed."
  :type '(choice (const :tag "Use projectile" projectile)
                 (const :tag "Use project.el" project-el))
  :group 'dashboard)

(defcustom dashboard-items '((recents   . 5)
                             (bookmarks . 5)
                             (agenda    . 5))
  "Association list of items to show in the startup buffer.
Will be of the form `(list-type . list-size)'.
If nil it is disabled.  Possible values for list-type are:
`recents' `bookmarks' `projects' `agenda' `registers'."
  :type  '(repeat (alist :key-type symbol :value-type integer))
  :group 'dashboard)

(defcustom dashboard-item-shortcuts '((recents . "r")
                                      (bookmarks . "m")
                                      (projects . "p")
                                      (agenda . "a")
                                      (registers . "e"))
  "Association list of items and their corresponding shortcuts.
Will be of the form `(list-type . keys)' as understood by
`(kbd keys)'.  If nil, shortcuts are disabled.  If an entry's
value is nil, that item's shortcut is disbaled.  See
`dashboard-items' for possible values of list-type.'"
  :type '(repeat (alist :key-type symbol :value-type string))
  :group 'dashboard)

(defcustom dashboard-items-default-length 20
  "Length used for startup lists with otherwise unspecified bounds.
Set to nil for unbounded."
  :type  'integer
  :group 'dashboard)

(defcustom dashboard-heading-icons '((recents   . "history")
                                     (bookmarks . "bookmark")
                                     (agenda    . "calendar")
                                     (projects . "rocket")
                                     (registers . "database"))
  "Association list for the icons of the heading sections.
Will be of the form `(list-type . icon-name-string)`.
If nil it is disabled.  Possible values for list-type are:
`recents' `bookmarks' `projects' `agenda' `registers'"
  :type  '(repeat (alist :key-type symbol :value-type string))
  :group 'dashboard)

(defvar recentf-list nil)

(defvar dashboard-buffer-name)

;;
;; Faces
;;
(defface dashboard-text-banner
  '((t (:inherit font-lock-keyword-face)))
  "Face used for text banners."
  :group 'dashboard)

(defface dashboard-banner-logo-title
  '((t :inherit default))
  "Face used for the banner title."
  :group 'dashboard)

(defface dashboard-navigator
  '((t (:inherit font-lock-keyword-face)))
  "Face used for the navigator."
  :group 'dashboard)

(defface dashboard-heading
  '((t (:inherit font-lock-keyword-face)))
  "Face used for widget headings."
  :group 'dashboard)

(defface dashboard-footer
  '((t (:inherit font-lock-doc-face)))
  "Face used for widget headings."
  :group 'dashboard)

(define-obsolete-face-alias
  'dashboard-text-banner-face 'dashboard-text-banner "1.2.6")
(define-obsolete-face-alias
  'dashboard-banner-logo-title-face 'dashboard-banner-logo-title "1.2.6")
(define-obsolete-face-alias
  'dashboard-heading-face 'dashboard-heading "1.2.6")

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

(defun dashboard-get-shortcut (item)
  "Get the shortcut to be used for ITEM."
  (let ((elem (assq item dashboard-item-shortcuts)))
    (and elem (cdr elem))))

(defmacro dashboard-insert-shortcut (shortcut-char
                                     search-label
                                     &optional no-next-line)
  "Insert a shortcut SHORTCUT-CHAR for a given SEARCH-LABEL.
Optionally, provide NO-NEXT-LINE to move the cursor forward a line."
  (let* (;; Ensure punctuation and upper case in search string is not
         ;; used to construct the `defun'
         (name (downcase (replace-regexp-in-string
                          "[[:punct:]]+" "" (format "%s" search-label) nil nil nil)))
         ;; Ensure whitespace in e.g. "recent files" is replaced with dashes.
         (sym (intern (format "dashboard-jump-to-%s" (replace-regexp-in-string
                                                      "[[:blank:]]+" "-" name nil nil nil)))))
    `(progn
       (eval-when-compile (defvar dashboard-mode-map))
       (defun ,sym nil
         ,(concat
           "Jump to "
           name
           ".  This code is dynamically generated in `dashboard-insert-shortcut'.")
         (interactive)
         (unless (search-forward ,search-label (point-max) t)
           (search-backward ,search-label (point-min) t))
         ,@(unless no-next-line
             '((forward-line 1)))
         (back-to-indentation))
       (eval-after-load 'dashboard
         (define-key dashboard-mode-map ,shortcut-char ',sym)))))

(defun dashboard-append (msg &optional _messagebuf)
  "Append MSG to dashboard buffer.
If MESSAGEBUF is not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create dashboard-buffer-name)
    (goto-char (point-max))
    (let ((buffer-read-only nil)) (insert msg))))

(defun dashboard-modify-heading-icons (alist)
  "Append ALIST items to `dashboard-heading-icons' to modify icons."
  (dolist (icon alist)
    (add-to-list 'dashboard-heading-icons icon)))

(defun dashboard-insert-page-break ()
  "Insert a page break line in dashboard buffer."
  (dashboard-append dashboard-page-separator))

(defun dashboard-insert-heading (heading &optional shortcut)
  "Insert a widget HEADING in dashboard buffer, adding SHORTCUT if provided."
  (when (and (display-graphic-p)
             dashboard-set-heading-icons)
    ;; Try loading `all-the-icons'
    (unless (or (fboundp 'all-the-icons-octicon)
                (require 'all-the-icons nil 'noerror))
      (error "Package `all-the-icons' isn't installed"))

    (insert (cond
             ((string-equal heading "Recent Files:")
              (all-the-icons-octicon (cdr (assoc 'recents dashboard-heading-icons))
                                     :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
             ((string-equal heading "Bookmarks:")
              (all-the-icons-octicon (cdr (assoc 'bookmarks dashboard-heading-icons))
                                     :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
             ((or (string-equal heading "Agenda for today:")
                  (string-equal heading "Agenda for the coming week:"))
              (all-the-icons-octicon (cdr (assoc 'agenda dashboard-heading-icons))
                                     :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
             ((string-equal heading "Registers:")
              (all-the-icons-octicon (cdr (assoc 'registers dashboard-heading-icons))
                                     :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
             ((string-equal heading "Projects:")
              (all-the-icons-octicon (cdr (assoc 'projects dashboard-heading-icons))
                                     :height 1.2 :v-adjust 0.0 :face 'dashboard-heading))
             (t " ")))
    (insert " "))

  (insert (propertize heading 'face 'dashboard-heading))
  (when shortcut (insert (format " (%s)" shortcut))))

(defun dashboard-center-line (string)
  "Center a STRING accoring to it's size."
  (insert (make-string (max 0 (floor (/ (- dashboard-banner-length
                                           (+ (length string) 1)) 2))) ?\ )))

;;
;; BANNER
;;
(defun dashboard-insert-ascii-banner-centered (file)
  "Insert banner from FILE."
  (let ((ascii-banner
         (with-temp-buffer
           (insert-file-contents file)
           (let ((banner-width 0))
             (while (not (eobp))
               (let ((line-length (- (line-end-position) (line-beginning-position))))
                 (if (< banner-width line-length)
                     (setq banner-width line-length)))
               (forward-line 1))
             (goto-char 0)
             (let ((margin
                    (max 0 (floor (/ (- dashboard-banner-length banner-width) 2)))))
               (while (not (eobp))
                 (insert (make-string margin ?\ ))
                 (forward-line 1))))
           (buffer-string))))
    (put-text-property 0 (length ascii-banner) 'face 'dashboard-text-banner ascii-banner)
    (insert ascii-banner)))

(defun dashboard-insert-image-banner (banner)
  "Display an image BANNER."
  (when (file-exists-p banner)
    (let* ((title dashboard-banner-logo-title)
           (spec
            (if (image-type-available-p 'imagemagick)
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
        (dashboard-center-line title)
        (insert (format "%s\n\n" (propertize title 'face 'dashboard-banner-logo-title)))))))

;;
;; INIT INFO
;;
(defun dashboard-insert-init-info ()
  "Insert init info when `dashboard-set-init-info' is t."
  (when dashboard-set-init-info
    (dashboard-center-line dashboard-init-info)
    (insert (propertize dashboard-init-info 'face 'font-lock-comment-face))))

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
          ((stringp dashboard-startup-banner)
           (if (and (file-exists-p dashboard-startup-banner)
                    (or (string-suffix-p ".txt" dashboard-startup-banner)
                        (and (display-graphic-p)
                             (image-type-available-p (intern (file-name-extension
                                                              dashboard-startup-banner))))))
               dashboard-startup-banner
             (message "could not find banner %s, use default instead" dashboard-startup-banner)
             (dashboard-get-banner-path 1)))
          (t (dashboard-get-banner-path 1)))))

(defun dashboard-insert-banner ()
  "Insert Banner at the top of the dashboard."
  (goto-char (point-max))
  (let ((banner (dashboard-choose-banner))
        (buffer-read-only nil))
    (when banner
      (if (image-type-available-p (intern (file-name-extension banner)))
          (dashboard-insert-image-banner banner)
        (dashboard-insert-ascii-banner-centered banner))
      (dashboard-insert-navigator)
      (dashboard-insert-init-info))))

(defun dashboard-insert-navigator ()
  "Insert Navigator of the dashboard."
  (when (and dashboard-set-navigator dashboard-navigator-buttons)
    (dolist (line dashboard-navigator-buttons)
      (dolist (btn line)
        (let* ((icon (car btn))
               (title (cadr btn))
               (help (or (cadr (cdr btn)) ""))
               (action (or (cadr (cddr btn)) #'ignore))
               (face (or (cadr (cddr (cdr btn))) 'dashboard-navigator))
               (prefix (or (cadr (cddr (cddr btn))) (propertize "[" 'face face)))
               (suffix (or (cadr (cddr (cddr (cdr btn)))) (propertize "]" 'face face))))
          (widget-create 'item
                         :tag (concat
                               (when icon
                                 (propertize icon 'face
                                             (let ((prop-face (get-text-property 0 'face icon)))
                                               (if prop-face
                                                   `(:inherit ,prop-face :inherit ,face)
                                                 `(:inherit ,face)))))
                               (when (and icon title
                                          (not (string-equal icon ""))
                                          (not (string-equal title "")))
                                 (propertize " " 'face 'variable-pitch))
                               (when title (propertize title 'face face)))
                         :help-echo help
                         :action action
                         :button-face `(:underline nil)
                         :mouse-face 'highlight
                         :button-prefix prefix
                         :button-suffix suffix
                         :format "%[%t%]")
          (insert " ")))
      (let* ((width (current-column)))
        (beginning-of-line)
        (dashboard-center-line (make-string width ?\s))
        (end-of-line))
      (insert "\n"))
    (insert "\n")))

(defmacro dashboard-insert-section (section-name list list-size shortcut action &rest widget-params)
  "Add a section with SECTION-NAME and LIST of LIST-SIZE items to the dashboard.
SHORTCUT is the keyboard shortcut used to access the section.
ACTION is theaction taken when the user activates the widget button.
WIDGET-PARAMS are passed to the \"widget-create\" function."
  `(progn
     (dashboard-insert-heading ,section-name
                               (if (and ,list ,shortcut dashboard-show-shortcuts) ,shortcut))
     (if ,list
         (when (and (dashboard-insert-section-list
                     ,section-name
                     (dashboard-subseq ,list 0 ,list-size)
                     ,action
                     ,@widget-params)
                    ,shortcut)
           (dashboard-insert-shortcut ,shortcut ,section-name))
       (insert "\n    --- No items ---"))))

;;
;; Section list
;;
(defmacro dashboard-insert-section-list (section-name list action &rest rest)
  "Insert into SECTION-NAME a LIST of items, expanding ACTION and passing REST to widget creation."
  `(when (car ,list)
     (mapc
      (lambda (el)
        (let ((tag ,@rest))
          (insert "\n    ")

          (when (and (display-graphic-p)
                     dashboard-set-file-icons
                     (or (fboundp 'all-the-icons-icon-for-dir)
                         (require 'all-the-icons nil 'noerror)))
            (let* ((path (car (last (split-string ,@rest " - "))))
                   (icon (if (and (not (file-remote-p path))
                                  (file-directory-p path))
                             (all-the-icons-icon-for-dir path nil "")
                           (cond
                            ((string-equal ,section-name "Agenda for today:")
                             (all-the-icons-octicon "primitive-dot" :height 1.0 :v-adjust 0.01))
                            ((file-remote-p path)
                             (all-the-icons-octicon "radio-tower" :height 1.0 :v-adjust 0.01))
                            (t (all-the-icons-icon-for-file (file-name-nondirectory path)
                                                            :v-adjust -0.05))))))
              (setq tag (concat icon " " ,@rest))))

          (widget-create 'item
                         :tag tag
                         :action ,action
                         :button-face `(:underline nil)
                         :mouse-face 'highlight
                         :button-prefix ""
                         :button-suffix ""
                         :format "%[%t%]")))
      ,list)))

;; Footer
(defun dashboard-random-footer ()
  "Return a random footer from `dashboard-footer-messages'."
  (nth (random (length dashboard-footer-messages)) dashboard-footer-messages))

(defun dashboard-insert-footer ()
  "Insert footer of dashboard."
  (let ((footer (and dashboard-set-footer (dashboard-random-footer))))
    (when footer
      (insert "\n")
      (dashboard-center-line footer)
      (insert dashboard-footer-icon)
      (insert " ")
      (insert (propertize footer 'face 'dashboard-footer))
      (insert "\n"))))

;;
;; Recentf
;;
(defun dashboard-insert-recents (list-size)
  "Add the list of LIST-SIZE items from recently edited files."
  (recentf-mode)
  (dashboard-insert-section
   "Recent Files:"
   recentf-list
   list-size
   (dashboard-get-shortcut 'recents)
   `(lambda (&rest ignore) (find-file-existing ,el))
   (abbreviate-file-name el)))

;;
;; Bookmarks
;;
(defun dashboard-insert-bookmarks (list-size)
  "Add the list of LIST-SIZE items of bookmarks."
  (require 'bookmark)
  (dashboard-insert-section
   "Bookmarks:"
   (dashboard-subseq (bookmark-all-names) 0 list-size)
   list-size
   (dashboard-get-shortcut 'bookmarks)
   `(lambda (&rest ignore) (bookmark-jump ,el))
   (let ((file (bookmark-get-filename el)))
     (if file
         (format "%s - %s" el (abbreviate-file-name file))
       el))))

;;
;; Projects
;;
(defun dashboard-insert-projects (list-size)
  "Add the list of LIST-SIZE items of projects."
  (cond
   ((eq dashboard-projects-backend 'projectile)
    (require 'projectile)
    (let ((inhibit-message t) (message-log-max nil))
      (projectile-cleanup-known-projects))
    (projectile-load-known-projects)
    (dashboard-insert-section
     "Projects:"
     (dashboard-subseq (projectile-relevant-known-projects)
                       0 list-size)
     list-size
     (dashboard-get-shortcut 'projects)
     `(lambda (&rest ignore) (projectile-switch-project-by-name ,el))
     (abbreviate-file-name el)))
   ((eq dashboard-projects-backend 'project-el)
    (require 'project)
    (dashboard-insert-section
     "Projects:"
     (dashboard-subseq (project-known-project-roots) 0 list-size)
     list-size
     (dashboard-get-shortcut 'projects)
     `(lambda (&rest ignore)
        (let ((default-directory ,el)
              (project-current-inhibit-prompt t))
          (call-interactively 'project-find-file)))
     (abbreviate-file-name el)))
   (t
    (display-warning '(dashboard)
                     "Invalid value for `dashboard-projects-backend'"
                     :error))))

;;
;; Org Agenda
;;
(defcustom dashboard-week-agenda t
  "Show agenda weekly if its not nil."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-agenda-time-string-format "%Y-%m-%d"
  "Format time of agenda entries."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-match-agenda-entry nil
  "Match agenda to extra filter.
It is the MATCH attribute for `org-map-entries'"
  :type 'string
  :group 'dashboard)

(defun dashboard-agenda-entry-time (schedule-time)
  "Format SCHEDULE-TIME with custom format.
If SCHEDULE-TIME is nil returns a blank string which length
is todays date format."
  (let* ((time (or schedule-time (org-today)))
         (formated-time (format-time-string
                         dashboard-agenda-time-string-format time)))
    (if schedule-time
        formated-time
      (replace-regexp-in-string "." " " formated-time))))

(defun dashboard-format-agenda-entry ()
  "Format agenda entry to show it on dashboard."
  (let* ((schedule-time (org-get-scheduled-time (point)))
         (deadline-time (org-get-deadline-time (point)))
         (item (org-agenda-format-item
                (dashboard-agenda-entry-time  (or schedule-time deadline-time))
                (org-get-heading)
                (org-outline-level)
                (org-get-category)
                (org-get-tags)
                t))
         (loc (point))
         (file (buffer-file-name)))
    (list item schedule-time deadline-time loc file)))

(defun dashboard-due-date-for-agenda ()
  "Return due-date for agenda period."
  (if dashboard-week-agenda
      (time-add (current-time) (* 86400 8))
    (time-add (current-time) 86400)))

(defun dashboard-filter-agenda-by-time ()
  "Include entry if it has a schedule-time or deadline-time in the future.
An entry is included if this function returns nil and excluded
if returns a point."
  (let ((schedule-time (org-get-scheduled-time (point)))
        (deadline-time (org-get-deadline-time (point)))
        (due-date (dashboard-due-date-for-agenda)))
    (if (or (org-entry-is-done-p)
            (and (null schedule-time)
                 (null deadline-time))
            (not (or (org-time-less-p deadline-time due-date)
                     (org-time-less-p schedule-time due-date))))
        (point)
      nil)))

(defun dashboard-no-filter-agenda ()
  "No filter agenda entries."
  (when (org-entry-is-done-p) (point)))

(defcustom dashboard-filter-agenda-entry `dashboard-filter-agenda-by-time
  "Function to filter `org-agenda' entries."
  :type 'function
  :group 'dashboard)

(defun dashboard-get-agenda ()
  "Get agenda items for today or for a week from now."
  (org-compile-prefix-format 'agenda)
  (org-map-entries `dashboard-format-agenda-entry
                   dashboard-match-agenda-entry
                   'agenda
                   dashboard-filter-agenda-entry))

(defun dashboard-insert-agenda (list-size)
  "Add the list of LIST-SIZE items of agenda."
  (require 'org-agenda)
  (let ((agenda (dashboard-get-agenda)))
    (dashboard-insert-section
     (if dashboard-week-agenda
         "Agenda for the coming week:"
       "Agenda for today:")
     agenda
     list-size
     (dashboard-get-shortcut 'agenda)
     `(lambda (&rest ignore)
        (let ((buffer (find-file-other-window (nth 4 ',el))))
          (with-current-buffer buffer (goto-char (nth 3 ',el)))
          (switch-to-buffer buffer)))
     (format "%s" (nth 0 el)))))

;;
;; Registers
;;
(defun dashboard-insert-registers (list-size)
  "Add the list of LIST-SIZE items of registers."
  (require 'register)
  (dashboard-insert-section
   "Registers:"
   register-alist
   list-size
   (dashboard-get-shortcut 'registers)
   (lambda (&rest _ignore) (jump-to-register (car el)))
   (format "%c - %s" (car el) (register-describe-oneline (car el)))))

(provide 'dashboard-widgets)
;;; dashboard-widgets.el ends here
