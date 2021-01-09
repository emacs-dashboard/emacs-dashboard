;;
;; (@* "Prepare" )
;;

(add-to-list 'load-path "./")
(add-to-list 'load-path "./scripts/")
(require 'pkg-prepare)

(jcs-ensure-package-installed '(elisp-lint org page-break-lines all-the-icons) t)

;;
;; (@* "Test" )
;;

(require 'dashboard)
(dashboard-insert-startupify-lists)
