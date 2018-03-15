EMACS ?= emacs

all: speeddating.elc

speeddating.elc : speeddating.el
	$(EMACS) -Q --batch --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile $<

clean:
	@rm -f speeddating.elc
