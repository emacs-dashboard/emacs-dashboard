export BATCH = --batch -q -l .emacs/init.el

EMACS ?= emacs
CASK ?= cask

ELLP := $(shell find . -regex '.*elisp-lint-[0-9]+\.[0-9]+')
ELS = $(filter-out emacs-dashboard-autoloads.el,$(wildcard *.el))
OBJECTS = $(ELS:.el=.elc)
BACKUPS = $(ELS:.el=.el~)

.PHONY: lint clean compile

lint:
	$(EMACS) $(BATCH) -l $(ELLP)/elisp-lint.el -f elisp-lint-files-batch --no-package-lint $(ELS)

clean:
	rm -rf .cask *.elc

compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . \
		--eval '(setq byte-compile-error-on-warn nil)' \
		-f batch-byte-compile $(ELS)
