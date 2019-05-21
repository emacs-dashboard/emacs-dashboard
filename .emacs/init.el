;;; Copied from https://github.com/gonewest818/elisp-lint
;;; init.el - Emacs initialization for isolated package testing
;;
;; Usage: emacs -q -l $project_root/emacs/init.el

(setq coding-system-for-write 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set `user-emacs-directory' to avoid overwriting $HOME/.emacs.d
;; See also: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=15539#66

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make sure customize data doesn't land in this file

(setq custom-file (concat user-emacs-directory ".emacs-custom.el"))
(when (file-readable-p custom-file) (load custom-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set load-path to include the .el files for the project under development,
;; which reside in the parent of `user-emacs-directory'.  Adding this path
;; permits tests to require this package itself.
(add-to-list 'load-path (expand-file-name ".." user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure melpa and melpa-stable

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa"        . "https://melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)
(when (not package-archive-contents)
    (package-refresh-contents))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap `use-package'

;; By default these will install from melpa anyway, but using
;; `package-pinned-packages' allows one to pin to melpa-stable
;; if necessary

(setq package-pinned-packages
      '((use-package        . "melpa")))

(dolist (p (mapcar 'car package-pinned-packages))
  (unless (package-installed-p p)
    (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load project dependencies from elsewhere

(load (concat user-emacs-directory "dependencies.el"))

;;; init.el ends here
