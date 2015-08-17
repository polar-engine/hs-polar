EXECUTABLE=polar4
CFLAGS=-W
DEBUGFLAGS=-prof -fprof-auto-calls
RELEASEFLAGS=-O2

.PHONY: debug
debug:
	ghc -isrc src/Main -o "$(EXECUTABLE)" $(CFLAGS) $(DEBUGFLAGS)

.PHONY: release
release:
	ghc -isrc src/Main -o "$(EXECUTABLE)" $(CFLAGS) $(RELEASEFLAGS)

.PHONY: clean
clean:
	rm -fv "$(EXECUTABLE)"
	find src -name '*.o' -print0 | xargs -0 rm -fv
	find src -name '*.hi' -print0 | xargs -0 rm -fv

.PHONY: test
test:
	runhaskell -isrc src/Test
