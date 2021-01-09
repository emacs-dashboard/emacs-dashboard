@echo off

echo "Executing package..."

emacs -nw --batch --load "./scripts/test-pkg.el"
