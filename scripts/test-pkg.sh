echo "Executing package..."

${EMACS:=emacs} -nw --batch --load "./scripts/test-pkg.el"
