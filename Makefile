all:
	cabal sandbox init
	cabal install

test:
	cabal configure --enable-tests -fF00 && cabal build && cabal test
