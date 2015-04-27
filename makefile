UNS := $(shell uname -s)
PM := brew cask
ifeq ($(UNS), "Linux")
	PM := apt
endif

test:
	./HsRPCGen.hs <spec.json

run:
	open ./TestGen.xcworkspace # TODO: xcbuild

lint:
	hlint *hs

deps:
	$(PM) install haskell-platform
	# or install manually from https://www.haskell.org/platform/
	cabal update
	cabal install json base-unicode-symbols

