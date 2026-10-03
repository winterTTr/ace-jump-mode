EMACS ?= emacs
BATCH = $(EMACS) -Q -batch -L .

.PHONY: all compile test clean

all: compile test

# Any byte compiler warning fails the build.
compile:
	$(BATCH) --eval '(setq byte-compile-error-on-warn t)' \
	         -f batch-byte-compile ace-jump-mode.el

# Load the newer of .el and .elc, so that a stale .elc never shadows
# the source.
test:
	$(BATCH) -L test --eval '(setq load-prefer-newer t)' \
	         -l ace-jump-mode-test -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc test/*.elc
