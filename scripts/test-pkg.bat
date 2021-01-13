@echo off

echo "Executing package..."

emacs -nw --batch --load "./scripts/test-pkg.el"

git clone "https://github.com/gonewest818/elisp-lint"

emacs --batch -q -l "./scripts/pkg-prepare.el" -l "./elisp-lint/elisp-lint.el" -f elisp-lint-files-batch --no-package-lint dashboard-widgets.el dashboard.el
