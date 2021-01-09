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
