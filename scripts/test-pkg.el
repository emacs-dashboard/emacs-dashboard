;;
;; (@* "Prepare" )
;;

(add-to-list 'load-path "./")
(load-file (expand-file-name "./scripts/pkg-prepare.el"))

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
