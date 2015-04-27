PM := brew cask
ifeq ($(shell uname -s), Linux)
PM = sudo apt
endif

test:
	./HsRPCGen.hs <spec.json

run:
	open ./TestGen.xcworkspace # TODO: xcbuild

lint:
	cabal install hlint
	hlint *hs

deps:
	$(PM) install haskell-platform
	# or install manually from https://www.haskell.org/platform/
	cabal update
	cabal install json base-unicode-symbols

