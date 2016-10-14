opam install core
opam install menhir
corebuild -use-menhir src/parser.mli
corebuild -use-menhir src/tester.native
