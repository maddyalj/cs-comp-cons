if [[ $* == *-i* ]]
then
    opam install core
    opam install menhir
fi
corebuild -use-menhir src/parser.mli
corebuild -use-menhir src/tester.native
