;;
;; (@* "Prepare" )
;;

(add-to-list 'load-path "./")
(add-to-list 'load-path "./scripts/")
(require 'pkg-prepare)

(jcs-ensure-package-installed
 (append
  '(page-break-lines org all-the-icons)
  '(elisp-lint package-lint dash))
 t)

;;
;; (@* "Test" )
;;

(require 'dashboard)
(dashboard-insert-startupify-lists)

(shell-command "emacs --batch -q -l \"./elisp-lint/elisp-lint.el\" -f elisp-lint-files-batch --no-package-lint dashboard-widgets.el dashboard.el")
