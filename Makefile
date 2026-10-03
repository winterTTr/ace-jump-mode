EMACS ?= emacs
BATCH = $(EMACS) -Q -batch -L .

# package-lint is cloned from git rather than installed from a package
# archive.  Where GitHub is reachable over ssh only, use
#   make lint PACKAGE_LINT_REPO=git@github.com:purcell/package-lint.git
PACKAGE_LINT_REPO ?= https://github.com/purcell/package-lint.git
PACKAGE_LINT_DIR ?= .package-lint

.PHONY: all compile test lint clean

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

# checkdoc always exits with success: any output it gives is a failure.
lint: $(PACKAGE_LINT_DIR)
	@out=$$($(BATCH) --eval '(checkdoc-file "ace-jump-mode.el")' 2>&1); \
	 if [ -n "$$out" ]; then echo "$$out"; exit 1; fi
	$(BATCH) -L $(PACKAGE_LINT_DIR) -l package-lint \
	         -f package-lint-batch-and-exit ace-jump-mode.el

$(PACKAGE_LINT_DIR):
	git clone --depth 1 $(PACKAGE_LINT_REPO) $@

clean:
	rm -f *.elc test/*.elc
