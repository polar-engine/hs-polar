all:
	ghc -W -O2 src/Main -o polar4

clean:
	rm -fv $(EXECUTABLE)
	find src -name '*.o' -print0 | xargs -0 rm -fv
	find src -name '*.hi' -print0 | xargs -0 rm -fv
