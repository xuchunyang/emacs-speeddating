EMACS ?= emacs

all: version speeddating.elc

version:
	@printf "EMACS=$(EMACS)\n"
	@$(EMACS) --version | head -1

.PHONY: speeddating.elc
speeddating.elc: speeddating.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $<

clean:
	@rm -f speeddating.elc
