CL=abcl
CLFLAGS=--batch --load-system-file asdf

.PHONY: all
all: test

.PHONY: test
test:
	$(CL) $(CLFLAGS) --eval '(asdf:test-system :lang-util)'
