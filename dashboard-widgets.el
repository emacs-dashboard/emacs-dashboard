;;; dashboard-widgets.el --- A startup screen extracted from Spacemacs  -*- lexical-binding: t -*-

;; Copyright (c) 2016-2021 emacs-dashboard maintainers
;;
;; Author     : Rakan Al-Hneiti <rakan.alhneiti@gmail.com>
;; Maintainer : Jesús Martínez <jesusmartinez93@gmail.com>
;;              Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL        : https://github.com/emacs-dashboard/emacs-dashboard
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;; Created: October 05, 2016
;; Package-Version: 1.8.0-SNAPSHOT
;; Keywords: startup, screen, tools, dashboard
;; Package-Requires: ((emacs "26.1"))
;;; Commentary:

;; An extensible Emacs dashboard, with sections for
;; bookmarks, projects (projectile or project.el), org-agenda and more.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'image)

;; Compiler pacifier
(declare-function all-the-icons-icon-for-dir "ext:all-the-icons.el")
(declare-function all-the-icons-icon-for-file "ext:all-the-icons.el")
(declare-function all-the-icons-fileicon "ext:data-fileicons.el")
(declare-function all-the-icons-octicon "ext:data-octicons.el")
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
(declare-function org-today "ext:org.el")
(declare-function org-get-todo-face "ext:org.el")
(declare-function org-get-todo-state "ext:org.el")
(declare-function org-entry-is-todo-p "ext:org.el")
(declare-function org-release-buffers "ext:org.el")
(defalias 'org-time-less-p 'time-less-p)
(defvar org-level-faces)
(defvar org-agenda-new-buffers)
(defvar all-the-icons-dir-icon-alist)
(defvar package-activated-list)

(defcustom dashboard-page-separator "\n\n"
  "Separator to use between the different pages."
  :type 'string
  :group 'dashboard)

(defcustom dashboard-image-banner-max-height 0
  "Maximum height of banner image.

This setting applies only if Emacs supports image transforms or
compiled with Imagemagick support.  When value is non-zero the image
banner will be resized to the specified height in pixels, with aspect
ratio preserved."
  :type 'integer
  :group 'dashboard)

(defcustom dashboard-image-banner-max-width 0
  "Maximum width of banner image.

This setting applies if Emacs supports image transforms or compiled
with Imagemagick support.  When value is non-zero the image banner
will be resized to the specified width in pixels, with aspect ratio
preserved."
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
    "Happy coding!"
    "Vi Vi Vi, the editor of the beast"
    "Welcome to the church of Emacs"
    "While any text editor can save your files, only Emacs can save your soul"
    "I showed you my source code, pls respond")
  "A list of messages, one of which dashboard chooses to display."
  :type 'list
  :group 'dashboard)

(defcustom dashboard-show-shortcuts t
  "Whether to show shortcut keys for each section."
  :type 'boolean
  :group 'dashboard)

(defconst dashboard-banners-directory
  (concat (file-name-directory (locate-library "dashboard")) "/banners/")
  "Default banner directory.")

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
  (lambda ()
    (let ((package-count 0) (time (emacs-init-time)))
      (when (bound-and-true-p package-alist)
        (setq package-count (length package-activated-list)))
      (when (boundp 'straight--profile-cache)
        (setq package-count (+ (hash-table-size straight--profile-cache) package-count)))
      (if (zerop package-count)
          (format "Emacs started in %s" time)
        (format "%d packages loaded in %s" package-count time))))
  "Init info with packages loaded and init time."
  :type '(function string)
  :group 'dashboard)

(defcustom dashboard-footer
  (nth (random (1- (1+ (length dashboard-footer-messages)))) dashboard-footer-messages)
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
With the value `projectile', the projects widget uses the package
projectile (available in MELPA).  With the value `project-el',
the widget uses the package project (available in GNU ELPA).

To activate the projects widget, add e.g. `(projects . 10)' to
`dashboard-items' after making sure the necessary package is
installed."
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

(defcustom dashboard-item-names nil
  "Association list of item heading names.
When an item is nil or not present, the default name is used.
Will be of the form `(default-name . new-name)'.
Possible values for default-name are:
\"Recent Files:\" \"Bookmarks:\" \"Agenda for today:\",
\"Agenda for the coming week:\" \"Registers:\" \"Projects:\"."
  :type '(alist :key-type string :value-type string)
  :options '("Recent Files:" "Bookmarks:" "Agenda for today:"
             "Agenda for the coming week:" "Registers:" "Projects:")
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

(defcustom dashboard-path-style nil
  "Style to display path."
  :type '(choice
          (const :tag "No specify" nil)
          (const :tag "Truncate the beginning part of the path" truncate-beginning)
          (const :tag "Truncate the middle part of the path" truncate-middle)
          (const :tag "Truncate the end part of the path" truncate-end))
  :group 'dashboard)

(defcustom dashboard-path-max-length 70
  "Maximum length for path to display."
  :type 'integer
  :group 'dashboard)

(defcustom dashboard-path-shorten-string "..."
  "String the that displays in the center of the path."
  :type 'string
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

(defface dashboard-items-face
  '((t (:inherit widget-button)))
  "Face used for items."
  :group 'dashboard)

(defface dashboard-no-items-face
  '((t (:inherit widget-button)))
  "Face used for no items."
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
Uses `cl-subseq`, but accounts for end points greater than the size of the list.
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

  ;; Turn the inserted heading into an overlay, so that we may freely change
  ;; its name without breaking any of the functions that expect the default name.
  ;; If there isn't a suitable entry in `dashboard-item-names',
  ;; we fallback to using HEADING.  In that case we still want it to be an
  ;; overlay to maintain consistent behavior (such as the point movement)
  ;; between modified and default headings.
  (let ((ov (make-overlay (- (point) (length heading)) (point) nil t)))
    (overlay-put ov 'display (or (cdr (assoc heading dashboard-item-names)) heading))
    (overlay-put ov 'face 'dashboard-heading))
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

(defun dashboard--type-is-gif-p (image-path)
  "Return if image is a gif.
String -> bool.
Argument IMAGE-PATH path to the image."
  (eq 'gif (image-type image-path)))

(defun dashboard-insert-image-banner (banner)
  "Display an image BANNER."
  (when (file-exists-p banner)
    (let* ((title dashboard-banner-logo-title)
           (size-props
            (append (when (> dashboard-image-banner-max-width 0)
                      (list :max-width dashboard-image-banner-max-width))
                    (when (> dashboard-image-banner-max-height 0)
                      (list :max-height dashboard-image-banner-max-height))))
           (spec
            (cond ((dashboard--type-is-gif-p banner)
                   (create-image banner))
                  ((image-type-available-p 'imagemagick)
                   (apply 'create-image banner 'imagemagick nil size-props))
                  (t
                   (apply 'create-image banner nil nil
                          (when (and (fboundp 'image-transforms-p)
                                     (memq 'scale (funcall 'image-transforms-p)))
                            size-props)))))
           ;; TODO: For some reason, `elisp-lint' is reporting error void
           ;; function `image-size'.
           (size (when (fboundp 'image-size) (image-size spec)))
           (width (car size))
           (left-margin (max 0 (floor (- dashboard-banner-length width) 2))))
      (goto-char (point-min))
      (insert "\n")
      (insert (make-string left-margin ?\ ))
      (insert-image spec)
      (when (dashboard--type-is-gif-p banner) (image-animate spec 0 t))
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
    (let ((init-info (if (functionp dashboard-init-info)
                         (funcall dashboard-init-info)
                       dashboard-init-info)))
      (dashboard-center-line init-info)
      (insert (propertize init-info 'face 'font-lock-comment-face)))))

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
                         :button-face 'dashboard-items-face
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
       (insert (propertize "\n    --- No items ---" 'face 'dashboard-no-items-face)))))

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
                            ((or (string-equal ,section-name "Agenda for today:")
                                 (string-equal ,section-name "Agenda for the coming week:"))
                             (all-the-icons-octicon "primitive-dot" :height 1.0 :v-adjust 0.01))
                            ((file-remote-p path)
                             (all-the-icons-octicon "radio-tower" :height 1.0 :v-adjust 0.01))
                            (t (all-the-icons-icon-for-file (file-name-nondirectory path)
                                                            :v-adjust -0.05))))))
              (setq tag (concat icon " " ,@rest))))

          (widget-create 'item
                         :tag tag
                         :action ,action
                         :button-face 'dashboard-items-face
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
;; Truncate
;;
(defcustom dashboard-shorten-by-window-width nil
  "Shorten path by window edges."
  :type 'boolean
  :group 'dashboard)

(defcustom dashboard-shorten-path-offset 0
  "Shorten path offset on the edges."
  :type 'integer
  :group 'dashboard)

(defun dashboard-f-filename (path)
  "Return file name from PATH."
  (file-name-nondirectory path))

(defun dashboard-f-base (path)
  "Return directory name from PATH."
  (file-name-nondirectory (directory-file-name (file-name-directory path))))

(defun dashboard-shorten-path-beginning (path)
  "Shorten PATH from beginning if exceeding maximum length."
  (let* ((len-path (length path)) (len-rep (length dashboard-path-shorten-string))
         (len-total (- dashboard-path-max-length len-rep))
         front)
    (if (<= len-path dashboard-path-max-length) path
      (setq front (ignore-errors (substring path (- len-path len-total) len-path)))
      (if front (concat dashboard-path-shorten-string front) ""))))

(defun dashboard-shorten-path-middle (path)
  "Shorten PATH from middle if exceeding maximum length."
  (let* ((len-path (length path)) (len-rep (length dashboard-path-shorten-string))
         (len-total (- dashboard-path-max-length len-rep))
         (center (/ len-total 2))
         (end-back center)
         (start-front (- len-path center))
         back front)
    (if (<= len-path dashboard-path-max-length) path
      (setq back (substring path 0 end-back)
            front (ignore-errors (substring path start-front len-path)))
      (if front (concat back dashboard-path-shorten-string front) ""))))

(defun dashboard-shorten-path-end (path)
  "Shorten PATH from end if exceeding maximum length."
  (let* ((len-path (length path)) (len-rep (length dashboard-path-shorten-string))
         (len-total (- dashboard-path-max-length len-rep))
         back)
    (if (<= len-path dashboard-path-max-length) path
      (setq back (ignore-errors (substring path 0 len-total)))
      (if (and back (< 0 dashboard-path-max-length))
          (concat back dashboard-path-shorten-string) ""))))

(defun dashboard--get-base-length (path type)
  "Return the length of the base from the PATH by TYPE."
  (let* ((is-dir (file-directory-p path))
         (base (if is-dir (dashboard-f-base path) (dashboard-f-filename path)))
         (option (cl-case type
                   (recents 'dashboard-recentf-show-base)
                   (bookmarks 'dashboard-bookmarks-show-base)
                   (projects 'dashboard-projects-show-base)))
         (option-val (symbol-value option))
         base-len)
    (cond ((eq option-val 'align)
           (setq base-len (dashboard--align-length-by-type type)))
          ((null option-val) (setq base-len 0))
          (t (setq base-len (length base))))
    base-len))

(defun dashboard-shorten-path (path type)
  "Shorten the PATH by TYPE."
  (setq path (abbreviate-file-name path))
  (let ((dashboard-path-max-length
         (if (and dashboard-path-style dashboard-shorten-by-window-width)
             (- (window-width) (dashboard--get-base-length path type)
                dashboard-shorten-path-offset)
           dashboard-path-max-length)))
    (cl-case dashboard-path-style
      (truncate-beginning (dashboard-shorten-path-beginning path))
      (truncate-middle (dashboard-shorten-path-middle path))
      (truncate-end (dashboard-shorten-path-end path))
      (t path))))

(defun dashboard-shorten-paths (paths alist type)
  "Shorten all path from PATHS by TYPE and store it to ALIST."
  (let (lst-display abbrev (index 0))
    (setf (symbol-value alist) nil)  ; reset
    (dolist (item paths)
      (setq abbrev (dashboard-shorten-path item type)
            ;; Add salt here, and use for extraction.
            ;; See function `dashboard-extract-key-path-alist'.
            abbrev (format "%s|%s" index abbrev))
      ;; store `abbrev' as id; and `item' with value
      (push (cons abbrev item) (symbol-value alist))
      (push abbrev lst-display)
      (cl-incf index))
    (reverse lst-display)))

(defun dashboard-extract-key-path-alist (key alist)
  "Remove salt from KEY, and return true shorten path from ALIST."
  (let* ((key (car (assoc key alist))) (split (split-string key "|")))
    (nth 1 split)))

(defun dashboard-expand-path-alist (key alist)
  "Get the full path (un-shorten) using KEY from ALIST."
  (cdr (assoc key alist)))

(defun dashboard--generate-align-format (fmt len)
  "Return FMT after inserting align LEN."
  (let ((pos (1+ (string-match-p "%s" fmt))))
    (concat (substring fmt 0 pos)
            (concat "-" (number-to-string len))
            (substring fmt pos (length fmt)))))

(defun dashboard--align-length-by-type (type)
  "Return the align length by TYPE of the section."
  (let ((len-item (cdr (assoc type dashboard-items))) (count 0) (align-length -1)
        len-list base)
    (cl-case type
      (recents
       (require 'recentf)
       (setq len-list (length recentf-list))
       (while (and (< count len-item) (< count len-list))
         (setq base (nth count recentf-list)
               align-length (max align-length (length (dashboard-f-filename base))))
         (cl-incf count)))
      (bookmarks
       (let ((bookmarks-lst (bookmark-all-names)))
         (setq len-list (length bookmarks-lst))
         (while (and (< count len-item) (< count len-list))
           (setq base (nth count bookmarks-lst)
                 align-length (max align-length (length base)))
           (cl-incf count))))
      (projects
       (let ((projects-lst (dashboard-projects-backend-load-projects)))
         (setq len-list (length projects-lst))
         (while (and (< count len-item) (< count len-list))
           (setq base (nth count projects-lst)
                 align-length (max align-length (length (dashboard-f-base base))))
           (cl-incf count))))
      (t (error "Unknown type for align length: %s" type)))
    align-length))

;;
;; Recentf
;;
(defcustom dashboard-recentf-show-base nil
  "Show the base file name infront of it's path."
  :type '(choice
          (const :tag "Don't show the base infront" nil)
          (const :tag "Respect format" t)
          (const :tag "Align the from base" align))
  :group 'dashboard)

(defcustom dashboard-recentf-item-format "%s  %s"
  "Format to use when showing the base of the file name."
  :type 'string
  :group 'dashboard)

(defvar dashboard-recentf-alist nil
  "Alist records shorten's recent files and it's full paths.")

(defvar dashboard--recentf-cache-item-format nil
  "Cache to record the new generated align format.")

(defun dashboard-insert-recents (list-size)
  "Add the list of LIST-SIZE items from recently edited files."
  (setq dashboard--recentf-cache-item-format nil)
  (recentf-mode)
  (let ((inhibit-message t) (message-log-max nil)) (recentf-cleanup))
  (dashboard-insert-section
   "Recent Files:"
   (dashboard-shorten-paths recentf-list 'dashboard-recentf-alist 'recents)
   list-size
   (dashboard-get-shortcut 'recents)
   `(lambda (&rest ignore)
      (find-file-existing (dashboard-expand-path-alist ,el dashboard-recentf-alist)))
   (let* ((file (dashboard-expand-path-alist el dashboard-recentf-alist))
          (filename (dashboard-f-filename file))
          (path (dashboard-extract-key-path-alist el dashboard-recentf-alist)))
     (cond
      ((eq dashboard-recentf-show-base 'align)
       (unless dashboard--recentf-cache-item-format
         (let* ((len-align (dashboard--align-length-by-type 'recents))
                (new-fmt (dashboard--generate-align-format
                          dashboard-recentf-item-format len-align)))
           (setq dashboard--recentf-cache-item-format new-fmt)))
       (format dashboard--recentf-cache-item-format filename path))
      ((null dashboard-recentf-show-base) path)
      (t (format dashboard-recentf-item-format filename path))))))

;;
;; Bookmarks
;;
(defcustom dashboard-bookmarks-show-base t
  "Show the base file name infront of it's path."
  :type '(choice
          (const :tag "Don't show the base infront" nil)
          (const :tag "Respect format" t)
          (const :tag "Align the from base" align))
  :group 'dashboard)

(defcustom dashboard-bookmarks-item-format "%s - %s"
  "Format to use when showing the base of the file name."
  :type 'string
  :group 'dashboard)

(defvar dashboard--bookmarks-cache-item-format nil
  "Cache to record the new generated align format.")

(defun dashboard-insert-bookmarks (list-size)
  "Add the list of LIST-SIZE items of bookmarks."
  (require 'bookmark)
  (dashboard-insert-section
   "Bookmarks:"
   (dashboard-subseq (bookmark-all-names) 0 list-size)
   list-size
   (dashboard-get-shortcut 'bookmarks)
   `(lambda (&rest ignore) (bookmark-jump ,el))
   (if-let* ((filename el)
             (path (bookmark-get-filename el))
             (path-shorten (dashboard-shorten-path path 'bookmarks)))
       (cond
        ((eq dashboard-bookmarks-show-base 'align)
         (unless dashboard--bookmarks-cache-item-format
           (let* ((len-align (dashboard--align-length-by-type 'bookmarks))
                  (new-fmt (dashboard--generate-align-format
                            dashboard-bookmarks-item-format len-align)))
             (setq dashboard--bookmarks-cache-item-format new-fmt)))
         (format dashboard--bookmarks-cache-item-format filename path-shorten))
        ((null dashboard-bookmarks-show-base) path-shorten)
        (t (format dashboard-bookmarks-item-format filename path-shorten)))
     el)))

;;
;; Projects
;;
(defcustom dashboard-projects-switch-function
  nil
  "Custom function to switch to projects from dashboard.
If non-NIL, should be bound to a function with one argument.  The
function will be called with the root directory of the project to
switch to."
  :type '(choice (const :tag "Default" nil) function)
  :group 'dashboard)

(defcustom dashboard-projects-show-base nil
  "Show the project name infront of it's path."
  :type '(choice
          (const :tag "Don't show the base infront" nil)
          (const :tag "Respect format" t)
          (const :tag "Align the from base" align))
  :group 'dashboard)

(defcustom dashboard-projects-item-format "%s  %s"
  "Format to use when showing the base of the project name."
  :type 'string
  :group 'dashboard)

(defvar dashboard-projects-alist nil
  "Alist records the shorten's project paths and it's full paths.")

(defvar dashboard--projects-cache-item-format nil
  "Cache to record the new generated align format.")

(defun dashboard-insert-projects (list-size)
  "Add the list of LIST-SIZE items of projects."
  (setq dashboard--projects-cache-item-format nil)
  (dashboard-insert-section
   "Projects:"
   (dashboard-shorten-paths
    (dashboard-subseq (dashboard-projects-backend-load-projects) 0 list-size)
    'dashboard-projects-alist 'projects)
   list-size
   (dashboard-get-shortcut 'projects)
   `(lambda (&rest ignore)
      (funcall (dashboard-projects-backend-switch-function)
               (dashboard-expand-path-alist ,el dashboard-projects-alist)))
   (let* ((file (dashboard-expand-path-alist el dashboard-projects-alist))
          (filename (dashboard-f-base file))
          (path (dashboard-extract-key-path-alist el dashboard-projects-alist)))
     (cond
      ((eq dashboard-projects-show-base 'align)
       (unless dashboard--projects-cache-item-format
         (let* ((len-align (dashboard--align-length-by-type 'projects))
                (new-fmt (dashboard--generate-align-format
                          dashboard-projects-item-format len-align)))
           (setq dashboard--projects-cache-item-format new-fmt)))
       (format dashboard--projects-cache-item-format filename path))
      ((null dashboard-projects-show-base) path)
      (t (format dashboard-projects-item-format filename path))))))

(defun dashboard-projects-backend-load-projects ()
  "Depending on `dashboard-projects-backend' load corresponding backend.
Return function that returns a list of projects."
  (cond
   ((eq dashboard-projects-backend 'projectile)
    (require 'projectile)
    (let ((inhibit-message t) (message-log-max nil))
      (projectile-cleanup-known-projects))
    (projectile-load-known-projects))
   ((eq dashboard-projects-backend 'project-el)
    (require 'project)
    (project-known-project-roots))
   (t
    (display-warning '(dashboard)
                     "Invalid value for `dashboard-projects-backend'"
                     :error))))

(defun dashboard-projects-backend-switch-function ()
  "Return the function to switch to a project.
Custom variable `dashboard-projects-switch-function' variable takes preference
over custom backends."
  (or dashboard-projects-switch-function
      (cond
       ((eq dashboard-projects-backend 'projectile)
        'projectile-switch-project-by-name)
       ((eq dashboard-projects-backend 'project-el)
        (lambda (project)
          "This function is used to switch to `PROJECT'."
          (let ((default-directory project))
            (project-find-file))))
       (t
        (display-warning '(dashboard)
                         "Invalid value for `dashboard-projects-backend'"
                         :error)))))
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

(defcustom dashboard-agenda-release-buffers nil
  "If not nil use `org-release-buffers' after getting the entries."
  :type 'boolean
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

(defun dashboard-agenda-entry-format ()
  "Format agenda entry to show it on dashboard."
  (let* ((schedule-time (org-get-scheduled-time (point)))
         (deadline-time (org-get-deadline-time (point)))
         (item (org-agenda-format-item
                (dashboard-agenda-entry-time (or schedule-time deadline-time))
                (org-get-heading)
                (org-outline-level)
                (org-get-category)
                (org-get-tags)
                t))
         (loc (point))
         (file (buffer-file-name)))
    (dashboard-agenda--set-agenda-headline-face item)
    (list item loc file)))

(defun dashboard-agenda--set-agenda-headline-face (headline)
  "Set agenda faces to `HEADLINE' when face text property is nil."
  (let ((todo (org-get-todo-state))
        (org-level-face (nth (- (org-outline-level) 1) org-level-faces)))
    (dashboard-agenda--set-face-when-match org-level-face
                                           (org-get-heading t t t t)
                                           headline)
    (dashboard-agenda--set-face-when-match (org-get-todo-face todo)
                                           todo
                                           headline)))

(defun dashboard-agenda--set-face-when-match (face text entry)
  "Set `FACE' to match text between `TEXT' and `ENTRY'.
Do nothing if `TEXT' has already a face property or is nil."
  (let ((match-part (and text (string-match text entry))))
    (when (and match-part (null (get-text-property 0 'face text)))
      (add-face-text-property (match-beginning 0) (match-end 0)
                              face t entry))))

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
    (unless (and (not (org-entry-is-done-p))
                 (or (and schedule-time
                          (org-time-less-p schedule-time due-date))
                     (and deadline-time
                          (org-time-less-p deadline-time due-date))))
      (point))))

(defun dashboard-filter-agenda-by-todo ()
  "Include entry if it is todo and not done.
An entry is included if this function returns nil and excluded
if returns a point."
  (unless (and (org-entry-is-todo-p)
               (not (org-entry-is-done-p)))
    (point)))

(defun dashboard-no-filter-agenda ()
  "No filter agenda entries."
  (when (org-entry-is-done-p) (point)))

(defcustom dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-time
  "Function to filter `org-agenda' entries."
  :type '(choice
          (const :tag "No filter" dashboard-no-filter-agenda)
          (const :tag "Filter by time" dashboard-filter-agenda-by-time)
          (const :tag "Filter by todo" dashboard-filter-agenda-by-todo)
          (function :tag "Custom function"))
  :group 'dashboard)

(defun dashboard-get-agenda ()
  "Get agenda items for today or for a week from now."
  (org-compile-prefix-format 'agenda)
  (prog1 (org-map-entries 'dashboard-agenda-entry-format
                          dashboard-match-agenda-entry
                          'agenda
                          dashboard-filter-agenda-entry)
    (dashboard-agenda--release-buffers)))

(defun dashboard-agenda--release-buffers ()
  "Release agenda buffers buffers.
This is what `org-agenda-exit' do."
  (when dashboard-agenda-release-buffers
    (org-release-buffers org-agenda-new-buffers)
    (setq org-agenda-new-buffers nil)))

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
        (let ((buffer (find-file-other-window (nth 2 ',el))))
          (with-current-buffer buffer
            (goto-char (nth 1 ',el))
            (switch-to-buffer buffer))))
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
