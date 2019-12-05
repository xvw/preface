.PHONY: all test clean fmt

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

fmt:
	dune build @fmt --auto-promote
