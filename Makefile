export BATCH = --batch -q -l .emacs/init.el

EMACS ?= emacs
EASK ?= eask

ELLP := $(shell find . -regex '.*elisp-lint-[0-9]+\.[0-9]+')
ELS = $(filter-out emacs-dashboard-autoloads.el,$(wildcard *.el))
OBJECTS = $(ELS:.el=.elc)
BACKUPS = $(ELS:.el=.el~)

.PHONY: lint clean install compile

ci: install compile

lint:
	$(EMACS) $(BATCH) -l $(ELLP)/elisp-lint.el -f elisp-lint-files-batch --no-package-lint $(ELS)

clean:
	$(EASK) clean
	$(EASK) clean-elc

install:
	@echo "Installing..."
	$(EASK) install

compile:
	@echo "Compiling..."
	$(EASK) compile
