all: build test

build:
	opam install core
	opam install menhir
	corebuild -use-menhir src/parser.mli
	corebuild -use-menhir src/tester.native

test:
	bash test.sh

all-part5: build-part5 test-part5

build-part5:
	corebuild src/part5.native

test-part5:
	./part5.native
