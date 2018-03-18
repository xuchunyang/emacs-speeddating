EMACS ?= emacs

all: version speeddating.elc test

version:
	$(EMACS) --version | head -1

.PHONY: speeddating.elc
speeddating.elc: speeddating.el
	$(EMACS) -Q --batch --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $<

test:
	$(EMACS) -Q --batch -L . -l speeddating-tests.el -f ert-run-tests-batch-and-exit

clean:
	@rm -f speeddating.elc
