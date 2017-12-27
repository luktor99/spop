all: solver
	@true

solver: solver.hs
	ghc -O2 -o solver solver.hs && rm solver.o solver.hi

test: solver
	$(shell ./test.sh)

.PHONY: all test
