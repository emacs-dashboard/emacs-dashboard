;;; dashboard.el --- A startup screen extracted from Spacemacs  -*- lexical-binding: t -*-

;; Copyright (c) 2016-2020 Rakan Al-Hneiti <rakan.alhneiti@gmail.com>
;; Copyright (c) 2019-2021 Jesús Martínez <jesusmartinez93@gmail.com>
;; Copyright (c) 2020-2021 Shen, Jen-Chieh <jcs090218@gmail.com>
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

(require 'seq)
(require 'recentf)

(require 'dashboard-widgets)

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
    (define-key map (kbd "g") #'dashboard-refresh-buffer)
    (define-key map (kbd "}") #'dashboard-next-section)
    (define-key map (kbd "{") #'dashboard-previous-section)
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
  (whitespace-mode -1)
  (linum-mode -1)
  (when (>= emacs-major-version 26)
    (display-line-numbers-mode -1))
  (page-break-lines-mode 1)
  (setq inhibit-startup-screen t
        buffer-read-only t
        truncate-lines t))

(defgroup dashboard nil
  "Extensible startup screen."
  :group 'applications)

(defcustom dashboard-center-content nil
  "Whether to center content within the window."
  :type 'boolean
  :group 'dashboard)

(defconst dashboard-buffer-name "*dashboard*"
  "Dashboard's buffer name.")

(defvar dashboard--section-starts nil
  "List of section starting positions.")

(defvar dashboard-force-refresh nil
  "If non-nil, force refresh dashboard buffer.")

(defun dashboard-previous-section ()
  "Navigate back to previous section."
  (interactive)
  (let ((current-section-start nil)
        (current-position (point))
        (previous-section-start nil))
    (dolist (elt dashboard--section-starts)
      (when (and current-section-start
                 (not previous-section-start))
        (setq previous-section-start elt))
      (when (and (not current-section-start)
                 (< elt current-position))
        (setq current-section-start elt)))
    (goto-char (if (eq current-position current-section-start)
                   previous-section-start
                 current-section-start))))

(defun dashboard-next-section ()
  "Navigate forward to next section."
  (interactive)
  (let ((current-position (point))
        (next-section-start nil)
        (section-starts (reverse dashboard--section-starts)))
    (dolist (elt section-starts)
      (when (and (not next-section-start)
                 (> elt current-position))
        (setq next-section-start elt)))
    (when next-section-start
      (goto-char next-section-start))))

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
  (let ((line-move-visual nil)
        (goal-column nil))
    (line-move arg t))
  ;; We never want to move point into an invisible line.  Dashboard doesn’t
  ;; use invisible text currently but when it does we’re ready!
  (while (and (invisible-p (point))
              (not (if (and arg (< arg 0)) (bobp) (eobp))))
    (forward-char (if (and arg (< arg 0)) -1 1)))
  (beginning-of-line-text))

(defun dashboard-return ()
  "Hit return key in dashboard buffer."
  (interactive)
  (let ((start-ln (line-number-at-pos))
        (fd-cnt 0)
        (diff-line nil)
        (entry-pt nil))
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
  (let ((buffer-exists (buffer-live-p (get-buffer dashboard-buffer-name)))
        (recentf-is-on (recentf-enabled-p))
        (origial-recentf-list recentf-list)
        (dashboard-num-recents (or (cdr (assoc 'recents dashboard-items)) 0))
        (max-line-length 0))
    ;; disable recentf mode,
    ;; so we don't flood the recent files list with org mode files
    ;; do this by making a copy of the part of the list we'll use
    ;; let dashboard widgets change that
    ;; then restore the orginal list afterwards
    ;; (this avoids many saves/loads that would result from
    ;; disabling/enabling recentf-mode)
    (when recentf-is-on
      (setq recentf-list (seq-take recentf-list dashboard-num-recents)))
    (when (or dashboard-force-refresh
              (not (eq dashboard-buffer-last-width (window-width)))
              (not buffer-exists))
      (setq dashboard-banner-length (window-width)
            dashboard-buffer-last-width dashboard-banner-length)
      (with-current-buffer (get-buffer-create dashboard-buffer-name)
        (let ((buffer-read-only nil))
          (erase-buffer)
          (dashboard-insert-banner)
          (dashboard-insert-page-break)
          (setq dashboard--section-starts nil)
          (mapc (lambda (els)
                  (let* ((el (or (car-safe els) els))
                         (list-size
                          (or (cdr-safe els)
                              dashboard-items-default-length))
                         (item-generator
                          (cdr-safe (assoc el dashboard-item-generators))))
                    (add-to-list 'dashboard--section-starts (point))
                    (funcall item-generator list-size)
                    (when recentf-is-on
                      (setq recentf-list origial-recentf-list))
                    (setq max-line-length
                          (max max-line-length (dashboard-maximum-section-length)))
                    (dashboard-insert-page-break)))
                dashboard-items)
          (when dashboard-center-content
            (when dashboard--section-starts
              (goto-char (car (last dashboard--section-starts))))
            (let ((margin (floor (/ (max (- (window-width) max-line-length) 0) 2))))
              (while (not (eobp))
                (unless (string-suffix-p (thing-at-point 'line) dashboard-page-separator)
                  (insert (make-string margin ?\ )))
                (forward-line 1))))
          (dashboard-insert-footer))
        (goto-char (point-min))
        (dashboard-mode)))
    (when recentf-is-on
      (setq recentf-list origial-recentf-list))))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-size-change-functions 'dashboard-resize-on-hook)
            (dashboard-resize-on-hook)))

(defun dashboard-refresh-buffer ()
  "Refresh buffer."
  (interactive)
  (let ((dashboard-force-refresh t)) (dashboard-insert-startupify-lists))
  (switch-to-buffer dashboard-buffer-name))

(defun dashboard-resize-on-hook (&optional _)
  "Re-render dashboard on window size change."
  (let ((space-win (get-buffer-window dashboard-buffer-name))
        (frame-win (frame-selected-window)))
    (when (and space-win
               (not (window-minibuffer-p frame-win)))
      (with-selected-window space-win
        (dashboard-insert-startupify-lists)))))

;;;###autoload
(defun dashboard-setup-startup-hook ()
  "Setup post initialization hooks.
If a command line argument is provided,
assume a filename and skip displaying Dashboard."
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
