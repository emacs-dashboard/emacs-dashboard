export EMACS ?= emacs
export BATCH = --batch -q -l .emacs/init.el

ELL = .emacs/elpa/elisp-lint-0.2.0/elisp-lint.el
ELS = $(filter-out $(wildcard *.el))
OBJECTS = $(ELS:.el=.elc)
BACKUPS = $(ELS:.el=.el~)

.PHONY: version lint clean cleanelpa

.elpa:
	$(EMACS) $(BATCH)
	touch .elpa

version: .elpa
	$(EMACS) $(BATCH) --version

lint: .elpa
	$(EMACS) $(BATCH) -l $(ELL) -f elisp-lint-files-batch $(ELS)
	$(EMACS) $(BATCH) -l $(ELL) -f elisp-lint-files-batch \
	                  --no-byte-compile \
	                  --no-package-format \
	                  --no-checkdoc \
	                  --no-check-declare $(TESTS)

clean:
	rm -f $(OBJECTS) $(BACKUPS) emacs-dashboard-autoloads.el*

cleanelpa: clean
	rm -rf .emacs/elpa .emacs/quelpa .emacs/.emacs-custom.el* .elpa
