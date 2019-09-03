.PHONY: all test clean

all:
	dune build

test:
	dune runtest -j 1

doc:
	dune build @doc

clean:
	dune clean

utop: all
	dune utop
