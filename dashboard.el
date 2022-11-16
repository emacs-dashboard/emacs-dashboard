;;; dashboard.el --- A startup screen extracted from Spacemacs  -*- lexical-binding: t -*-

;; Copyright (c) 2016-2022 emacs-dashboard maintainers
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

(require 'ffap)
(require 'recentf)

(require 'dashboard-widgets)

(declare-function bookmark-get-filename "ext:bookmark.el")
(declare-function bookmark-all-names "ext:bookmark.el")
(declare-function dashboard-ls--dirs "ext:dashboard-ls.el")
(declare-function dashboard-ls--files "ext:dashboard-ls.el")
(declare-function page-break-lines-mode "ext:page-break-lines.el")
(declare-function projectile-remove-known-project "ext:projectile.el")
(declare-function project-forget-projects-under "ext:project.el")

(defgroup dashboard nil
  "Extensible startup screen."
  :group 'applications)

;; Custom splash screen
(defvar dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-p") 'dashboard-previous-line)
    (define-key map (kbd "C-n") 'dashboard-next-line)
    (define-key map (kbd "<up>") 'dashboard-previous-line)
    (define-key map (kbd "<down>") 'dashboard-next-line)
    (define-key map (kbd "k") 'dashboard-previous-line)
    (define-key map (kbd "j") 'dashboard-next-line)
    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "RET") 'dashboard-return)
    (define-key map [mouse-1] 'dashboard-mouse-1)
    (define-key map (kbd "}") #'dashboard-next-section)
    (define-key map (kbd "{") #'dashboard-previous-section)

    (define-key map (kbd "<backspace>") #'dashboard-remove-item-under)
    (define-key map (kbd "<delete>") #'dashboard-remove-item-under)
    (define-key map (kbd "DEL") #'dashboard-remove-item-under)

    (define-key map (kbd "1") #'dashboard-section-1)
    (define-key map (kbd "2") #'dashboard-section-2)
    (define-key map (kbd "3") #'dashboard-section-3)
    (define-key map (kbd "4") #'dashboard-section-4)
    (define-key map (kbd "5") #'dashboard-section-5)
    (define-key map (kbd "6") #'dashboard-section-6)
    (define-key map (kbd "7") #'dashboard-section-7)
    (define-key map (kbd "8") #'dashboard-section-8)
    (define-key map (kbd "9") #'dashboard-section-9)
    map)
  "Keymap for dashboard mode.")

(defcustom dashboard-after-initialize-hook nil
  "Hook that is run after dashboard buffer is initialized."
  :group 'dashboard
  :type 'hook)

(define-derived-mode dashboard-mode special-mode "Dashboard"
  "Dashboard major mode for startup screen."
  :group 'dashboard
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (when (featurep 'whitespace) (whitespace-mode -1))
  (when (featurep 'linum) (linum-mode -1))
  (when (featurep 'display-line-numbers) (display-line-numbers-mode -1))
  (when (featurep 'page-break-lines) (page-break-lines-mode 1))
  (setq-local revert-buffer-function #'dashboard-refresh-buffer)
  (setq inhibit-startup-screen t
        buffer-read-only t
        truncate-lines t))

(defcustom dashboard-center-content nil
  "Whether to center content within the window."
  :type 'boolean
  :group 'dashboard)

(defconst dashboard-buffer-name "*dashboard*"
  "Dashboard's buffer name.")

(defvar dashboard-force-refresh nil
  "If non-nil, force refresh dashboard buffer.")

(defvar dashboard--section-starts nil
  "List of section starting positions.")

;;
;; Util
;;
(defun dashboard--goto-line (line)
  "Goto LINE."
  (goto-char (point-min)) (forward-line (1- line)))

(defmacro dashboard--save-excursion (&rest body)
  "Execute BODY save window point."
  (declare (indent 0) (debug t))
  `(let ((line (line-number-at-pos nil t))
         (column (current-column)))
     ,@body
     (dashboard--goto-line line)
     (move-to-column column)))

;;
;; Core
;;
(defun dashboard--current-section ()
  "Return section symbol in dashboard."
  (save-excursion
    (if (and (search-backward dashboard-page-separator nil t)
             (search-forward dashboard-page-separator nil t))
        (let ((ln (thing-at-point 'line)))
          (cond ((string-match-p "Recent Files:" ln)     'recents)
                ((string-match-p "Bookmarks:" ln)        'bookmarks)
                ((string-match-p "Projects:" ln)         'projects)
                ((string-match-p "Agenda for " ln)       'agenda)
                ((string-match-p "Registers:" ln)        'registers)
                ((string-match-p "List Directories:" ln) 'ls-directories)
                ((string-match-p "List Files:" ln)       'ls-files)
                (t (user-error "Unknown section from dashboard"))))
      (user-error "Failed searching dashboard section"))))

;;
;; Navigation
;;
(defun dashboard-previous-section ()
  "Navigate back to previous section."
  (interactive)
  (let ((current-position (point)) current-section-start previous-section-start)
    (dolist (elt dashboard--section-starts)
      (when (and current-section-start (not previous-section-start))
        (setq previous-section-start elt))
      (when (and (not current-section-start) (< elt current-position))
        (setq current-section-start elt)))
    (goto-char (if (eq current-position current-section-start)
                   previous-section-start
                 current-section-start))))

(defun dashboard-next-section ()
  "Navigate forward to next section."
  (interactive)
  (let ((current-position (point)) next-section-start
        (section-starts (reverse dashboard--section-starts)))
    (dolist (elt section-starts)
      (when (and (not next-section-start)
                 (> elt current-position))
        (setq next-section-start elt)))
    (when next-section-start
      (goto-char next-section-start))))

(defun dashboard--section-lines ()
  "Return a list of integer represent the starting line number of each section."
  (let (pb-lst)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward dashboard-page-separator nil t)
        (when (ignore-errors (dashboard--current-section))
          (push (line-number-at-pos) pb-lst))))
    (setq pb-lst (reverse pb-lst))
    pb-lst))

(defun dashboard--goto-section-by-index (index)
  "Navigate to item section by INDEX."
  (let* ((pg-lst (dashboard--section-lines))
         (items-id (1- index))
         (items-pg (nth items-id pg-lst))
         (items-len (length pg-lst)))
    (when (and items-pg (< items-id items-len))
      (dashboard--goto-line items-pg))))

(defun dashboard-section-1 ()
  "Navigate to section 1." (interactive) (dashboard--goto-section-by-index 1))
(defun dashboard-section-2 ()
  "Navigate to section 2." (interactive) (dashboard--goto-section-by-index 2))
(defun dashboard-section-3 ()
  "Navigate to section 3." (interactive) (dashboard--goto-section-by-index 3))
(defun dashboard-section-4 ()
  "Navigate to section 4." (interactive) (dashboard--goto-section-by-index 4))
(defun dashboard-section-5 ()
  "Navigate to section 5." (interactive) (dashboard--goto-section-by-index 5))
(defun dashboard-section-6 ()
  "Navigate to section 6." (interactive) (dashboard--goto-section-by-index 6))
(defun dashboard-section-7 ()
  "Navigate to section 7." (interactive) (dashboard--goto-section-by-index 7))
(defun dashboard-section-8 ()
  "Navigate to section 8." (interactive) (dashboard--goto-section-by-index 8))
(defun dashboard-section-9 ()
  "Navigate to section 9." (interactive) (dashboard--goto-section-by-index 9))

(defun dashboard-previous-line (arg)
  "Move point up and position it at that line’s item.
Optional prefix ARG says how many lines to move; default is one line."
  (interactive "^p")
  (dashboard-next-line (- arg)))

(defun dashboard-next-line (arg)
  "Move point down and position it at that line’s item.
Optional prefix ARG says how many lines to move; default is one line."
  ;; code heavily inspired by `dired-next-line'
  (interactive "^p")
  (let (line-move-visual goal-column)
    (line-move arg t))
  ;; We never want to move point into an invisible line.  Dashboard doesn’t
  ;; use invisible text currently but when it does we’re ready!
  (while (and (invisible-p (point))
              (not (if (and arg (< arg 0)) (bobp) (eobp))))
    (forward-char (if (and arg (< arg 0)) -1 1)))
  (beginning-of-line-text))

;;
;; ffap
;;
(defun dashboard--goto-section (section)
  "Move to SECTION declares in variable `dashboard-item-shortcuts'."
  (let ((fnc (intern (format "dashboard-jump-to-%s" section))))
    (dashboard-funcall-fboundp fnc)))

(defun dashboard--current-index (section &optional pos)
  "Return the idex by SECTION from POS."
  (let (target-ln section-line)
    (save-excursion
      (when pos (goto-char pos))
      (setq target-ln (line-number-at-pos))
      (dashboard--goto-section section)
      (setq section-line (line-number-at-pos)))
    (- target-ln section-line)))

(defun dashboard--section-list (section)
  "Return the list from SECTION."
  (cl-case section
    (`recents recentf-list)
    (`bookmarks (bookmark-all-names))
    (`projects (dashboard-projects-backend-load-projects))
    (`ls-directories (dashboard-ls--dirs))
    (`ls-files (dashboard-ls--files))
    (t (user-error "Unknown section for search: %s" section))))

(defun dashboard--current-item-in-path ()
  "Return the path from current dashboard section in path."
  (let ((section (dashboard--current-section)) path)
    (cl-case section
      (`bookmarks (setq path (bookmark-get-filename path)))
      (t
       (let ((lst (dashboard--section-list section))
             (index (dashboard--current-index section)))
         (setq path (nth index lst)))))
    path))

(defun dashboard--on-path-item-p ()
  "Return non-nil if current point is on the item path from dashboard."
  (save-excursion
    (when (= (point) (line-end-position)) (ignore-errors (forward-char -1)))
    (eq (get-char-property (point) 'face) 'dashboard-items-face)))

(defun dashboard--ffap-guesser--adv (fnc &rest args)
  "Advice execution around function `ffap-guesser'.

Argument FNC is the adviced function.
Optional argument ARGS adviced function arguments."
  (cl-case major-mode
    (`dashboard-mode
     (or (and (dashboard--on-path-item-p)
              (dashboard--current-item-in-path))
         (apply fnc args)))  ; fallback
    (t (apply fnc args))))
(advice-add 'ffap-guesser :around #'dashboard--ffap-guesser--adv)

;;
;; Removal
;;
(defun dashboard-remove-item-under ()
  "Remove a item from the current item section."
  (interactive)
  (cl-case (dashboard--current-section)
    (`recents   (dashboard-remove-item-recentf))
    (`bookmarks (dashboard-remove-item-bookmarks))
    (`projects  (dashboard-remove-item-projects))
    (`agenda    (dashboard-remove-item-agenda))
    (`registers (dashboard-remove-item-registers)))
  (dashboard--save-excursion (dashboard-refresh-buffer)))

(defun dashboard-remove-item-recentf ()
  "Remove a file from `recentf-list'."
  (interactive)
  (let ((path (save-excursion (end-of-line) (ffap-guesser))))
    (setq recentf-list (delete path recentf-list)))
  (dashboard-mute-apply (recentf-save-list)))

(defun dashboard-remove-item-projects ()
  "Remove a path from `project--list'."
  (interactive)
  (let ((path (save-excursion (end-of-line) (ffap-guesser))))
    (dashboard-mute-apply
      (cl-case dashboard-projects-backend
        (`projectile (projectile-remove-known-project path))
        (`project-el (project-forget-projects-under path))))))

(defun dashboard-remove-item-bookmarks ()
  "Remove a bookmarks from `bookmark-alist'."
  (interactive))  ; TODO: ..

(defun dashboard-remove-item-agenda ()
  "Remove an agenda from `org-agenda-files'."
  (interactive "P")
  (let ((agenda-file (get-text-property (point) 'dashboard-agenda-file))
        (agenda-loc (get-text-property (point) 'dashboard-agenda-loc)))
    (with-current-buffer (find-file-noselect agenda-file)
      (goto-char agenda-loc)
      (call-interactively 'org-todo))))

(defun dashboard-remove-item-registers ()
  "Remove a registers from `register-alist'."
  (interactive))  ; TODO: ..

;;
;; Confirmation
;;
(defun dashboard-return ()
  "Hit return key in dashboard buffer."
  (interactive)
  (let ((start-ln (line-number-at-pos)) (fd-cnt 0) diff-line entry-pt)
    (save-excursion
      (while (and (not diff-line)
                  (not (= (point) (point-min)))
                  (not (get-char-property (point) 'button))
                  (not (= (point) (point-max))))
        (forward-char 1)
        (setq fd-cnt (1+ fd-cnt))
        (unless (= start-ln (line-number-at-pos))
          (setq diff-line t)))
      (unless (= (point) (point-max))
        (setq entry-pt (point))))
    (when (= fd-cnt 1)
      (setq entry-pt (1- (point))))
    (if entry-pt
        (widget-button-press entry-pt)
      (call-interactively #'widget-button-press))))

(defun dashboard-mouse-1 ()
  "Key for keymap `mouse-1'."
  (interactive)
  (let ((old-track-mouse track-mouse))
    (when (call-interactively #'widget-button-click)
      (setq track-mouse old-track-mouse))))

;;
;; Insertion
;;
(defun dashboard-maximum-section-length ()
  "For the just-inserted section, calculate the length of the longest line."
  (let ((max-line-length 0))
    (save-excursion
      (dashboard-previous-section)
      (while (not (eobp))
        (setq max-line-length
              (max max-line-length
                   (- (line-end-position) (line-beginning-position))))
        (forward-line 1)))
    max-line-length))

(defun dashboard-insert-startupify-lists ()
  "Insert the list of widgets into the buffer."
  (interactive)
  (let ((recentf-is-on (recentf-enabled-p))
        (origial-recentf-list recentf-list)
        (dashboard-num-recents (or (cdr (assoc 'recents dashboard-items)) 0))
        (max-line-length 0))
    (when recentf-is-on
      (setq recentf-list (dashboard-subseq recentf-list dashboard-num-recents)))
    (prog1
        (with-current-buffer (get-buffer-create dashboard-buffer-name)
          (when (or dashboard-force-refresh (not (eq major-mode 'dashboard-mode)))
            (let (buffer-read-only)
              (erase-buffer)
              (dashboard-insert-banner)
              (setq dashboard--section-starts nil)
              (mapc (lambda (els)
                      (let* ((el (or (car-safe els) els))
                             (list-size
                              (or (cdr-safe els)
                                  dashboard-items-default-length))
                             (item-generator
                              (cdr-safe (assoc el dashboard-item-generators))))
                        (push (point) dashboard--section-starts)
                        (funcall item-generator list-size)
                        (goto-char (point-max))
                        (when recentf-is-on
                          (setq recentf-list origial-recentf-list))
                        (setq max-line-length
                              (max max-line-length (dashboard-maximum-section-length)))))
                    dashboard-items)
              (when dashboard-center-content
                (dashboard-center-text
                 (if dashboard--section-starts
                     (car (last dashboard--section-starts))
                   (point))
                 (point-max)))
              (insert dashboard-page-separator)
              (save-excursion
                (dolist (start dashboard--section-starts)
                  (goto-char start)
                  (insert dashboard-page-separator)))
              (dashboard-insert-footer))
            (goto-char (point-min))
            (dashboard-mode))
          (current-buffer))
      (when recentf-is-on
        (setq recentf-list origial-recentf-list)))))

(defun dashboard-refresh-buffer (&rest _)
  "Refresh buffer."
  (interactive)
  (let ((dashboard-force-refresh t)) (dashboard-insert-startupify-lists))
  (switch-to-buffer dashboard-buffer-name))

;;;###autoload
(defun dashboard-setup-startup-hook ()
  "Setup post initialization hooks.
If a command line argument is provided, assume a filename and skip displaying
Dashboard."
  (when (< (length command-line-args) 2)
    (add-hook 'after-init-hook (lambda ()
                                 ;; Display useful lists of items
                                 (dashboard-insert-startupify-lists)))
    (add-hook 'emacs-startup-hook (lambda ()
                                    (switch-to-buffer dashboard-buffer-name)
                                    (goto-char (point-min))
                                    (redisplay)
                                    (run-hooks 'dashboard-after-initialize-hook)))))

(provide 'dashboard)
;;; dashboard.el ends here
