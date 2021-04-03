.PHONY: all test clean utop fmt

all:
	dune build

test:
	dune runtest --no-buffer -j 1

doc:
	dune build @doc

clean:
	dune clean

utop: all
	dune utop

fmt:
	dune build @fmt --auto-promote

%.svg: ./.github/dot/%.dot
	  dot -Tsvg $(<) -o ./.github/figures/$(@)

dot: specs.svg cut.svg
