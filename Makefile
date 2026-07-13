CASK ?= cask

.PHONY: all deps test compile clean

all: test

## Fetch development dependencies (buttercup) into the Cask sandbox.
deps:
	$(CASK) install

## Run the buttercup test suite.
test:
	$(CASK) exec buttercup -L . tests

## Byte-compile the package.
compile:
	$(CASK) build

## Remove byte-compiled files.
clean:
	$(CASK) clean-elc
