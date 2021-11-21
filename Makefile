.PHONY: all test clean utop fmt

all:
	dune build

test:
	dune runtest --no-buffer -j 1

clean:
	dune clean

utop: all
	dune utop

fmt:
	dune build @fmt --auto-promote

%.svg: ./.github/dot/%.dot
	mkdir -p ./.github/figures
	dot -Tsvg $(<) -o ./.github/figures/$(@)

dot: specs.svg cut.svg


doc: dot
	dune build @doc
	mkdir -p _build/default/_doc/_html/images
	cp .github/figures/* _build/default/_doc/_html/images
