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
;; Modified: December 30, 2016
;; Version: 1.2.2
;; Keywords: startup screen tools
;; Package-Requires: ((emacs "24.4") (page-break-lines "0.11"))
;;; Commentary:

;; An extensible Emacs dashboard, with sections for
;; bookmarks, projectil projects, org-agenda and more.

;;; Code:

(require 'bookmark)
(require 'calendar)
(require 'org-agenda)
(require 'page-break-lines)
(require 'recentf)

(require 'dashboard-widgets)

;; Custom splash screen
(defvar dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map (kbd "g") #'dashboard-refresh-buffer)
    (define-key map (kbd "}") #'dashboard-next-section)
    (define-key map (kbd "{") #'dashboard-previous-section)
    map)
  "Keymap for dashboard mode.")

(define-derived-mode dashboard-mode special-mode "Dashboard"
  "Dashboard major mode for startup screen.
\\<dashboard-mode-map>
"
  :group 'dashboard
  :syntax-table nil
  :abbrev-table nil
  (whitespace-mode -1)
  (linum-mode -1)
  (page-break-lines-mode 1)
  (setq inhibit-startup-screen t)
  (setq buffer-read-only t
        truncate-lines t))

(defgroup dashboard nil
  "Settings that are used in the Dashboard"
  :group 'dashboard)

(defconst dashboard-buffer-name "*dashboard*"
	  "Dashboard's buffer name.")

(defvar dashboard--section-starts nil
  "List of section starting positions")

(defun dashboard-previous-section ()
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
  (interactive
   (let ((current-position (point))
         (next-section-start nil)
         (section-starts (reverse dashboard--section-starts)))
     (dolist (elt section-starts)
       (when (and (not next-section-start)
                  (> elt current-position))
         (setq next-section-start elt)))
     (when next-section-start
       (goto-char next-section-start)))))

(defun dashboard-insert-startupify-lists ()
  "Insert the list of widgets into the buffer."
  (interactive)
  (let ((buffer-exists (buffer-live-p (get-buffer dashboard-buffer-name)))
        (save-line nil))
    (when (or (not (eq dashboard-buffer-last-width (window-width)))
              (not buffer-exists))
      (setq dashboard-banner-length (window-width)
            dashboard-buffer-last-width dashboard-banner-length)
      (with-current-buffer (get-buffer-create dashboard-buffer-name)
        (let ((buffer-read-only nil)
              (list-separator "\n\n"))
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
                    (dashboard-insert-page-break)))
                dashboard-items))
        (dashboard-mode)
        (goto-char (point-min))))))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-configuration-change-hook 'dashboard-resize-on-hook)
            (dashboard-resize-on-hook)))

(defun dashboard-refresh-buffer ()
  "Refresh buffer."
  (interactive)
  (kill-buffer dashboard-buffer-name)
  (dashboard-insert-startupify-lists)
  (switch-to-buffer dashboard-buffer-name))

(defun dashboard-resize-on-hook ()
  (let ((space-win (get-buffer-window dashboard-buffer-name))
        (frame-win (frame-selected-window)))
    (when (and space-win
               (not (window-minibuffer-p frame-win)))
      (with-selected-window space-win
        (dashboard-insert-startupify-lists)))))

;;;###autoload
(defun dashboard-setup-startup-hook ()
  "Setup post initialization hooks.
If a command line argument is provided, assume a filename and skip displaying Dashboard"
  (if (< (length command-line-args) 2 )
      (progn
	(add-hook 'after-init-hook (lambda ()
				     ;; Display useful lists of items
				     (dashboard-insert-startupify-lists)))
	(add-hook 'emacs-startup-hook '(lambda ()
					 (switch-to-buffer "*dashboard*")
					 (goto-char (point-min))
					 (redisplay))))))

(provide 'dashboard)
;;; dashboard.el ends here
