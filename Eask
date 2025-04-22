;; -*- mode: eask; lexical-binding: t -*-

(package "dashboard"
         "1.9.0snapshot"
         "A startup screen extracted from Spacemacs")

(website-url "https://github.com/emacs-dashboard/emacs-dashboard")
(keywords "startup" "screen" "tools" "dashboard")

(package-file "dashboard.el")
(files "dashboard-widgets.el" "*.org" "banners")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "27.1")

(development
 (depends-on "elisp-lint")
 (depends-on "page-break-lines"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

(add-hook 'eask-before-compile-hook
           (lambda (&rest _)
             (setq byte-compile-error-on-warn t)))
