UNS := $(shell uname -s)
PM := brew
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
	$(PM) install ghc cabal-install
	cabal update
	cabal install json base-unicode-symbols

