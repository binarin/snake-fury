test: build
    cabal repl --with-compiler=doctest

build:
    cabal build
