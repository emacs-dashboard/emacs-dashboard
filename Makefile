export EMACS ?= emacs
export BATCH = --batch -q -l .emacs/init.el

EMACS ?= emacs
CASK ?= cask

ELLP := $(shell find . -regex '.*elisp-lint-[0-9]+\.[0-9]+')
ELS = $(filter-out emacs-dashboard-autoloads.el,$(wildcard *.el))
OBJECTS = $(ELS:.el=.elc)
BACKUPS = $(ELS:.el=.el~)

.PHONY: version lint clean cleanelpa

lint:
	$(EMACS) $(BATCH) -l $(ELLP)/elisp-lint.el -f elisp-lint-files-batch --no-package-lint $(ELS)

clean:
	rm -f $(OBJECTS) $(BACKUPS) emacs-dashboard-autoloads.el*

cleanelpa: clean
	rm -rf .emacs/elpa .emacs/quelpa .emacs/.emacs-custom.el* .elpa

compile:
	@echo "Compiling..."
	@$(CASK) $(EMACS) -Q --batch \
		-L . \
		--eval '(setq byte-compile-error-on-warn t)' \
		-f batch-byte-compile $(ELS)
