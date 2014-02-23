all:
	cabal install -fdevelopment -j && ./.cabal-sandbox/bin/classwar
