(source "gnu")
(source "melpa")

(package "dashboard" "1.8.0_SNAPSHOT" "A startup screen extracted from Spacemacs")

(package-file "dashboard.el")
(files "*.el" "*.org" "banners")

(depends-on "emacs" "26.1")

(development
 (depends-on "elisp-lint")
 (depends-on "page-break-lines"))

(setq network-security-level 'low      ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
      byte-compile-error-on-warn nil)
