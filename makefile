PM := brew cask
ifeq ($(shell uname -s), Linux)
PM = sudo apt
endif

all: deps
	ghc --make HsRPCGen.hs -o HsRPCGen

test:
	./hsrpcgen.sh <spec.json

run:
	open ./TestGen.xcworkspace # TODO: xcbuild

lint:
	cabal install hlint
	~/.cabal/bin/hlint *hs

deps:
	$(PM) install haskell-platform
	# or install manually from https://www.haskell.org/platform/
	cabal update
	cabal install json base-unicode-symbols

clean:
	rm -f *\.hi
	rm -f *\.o
