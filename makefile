PM := brew cask
ifeq ($(shell uname -s), Linux)
PM = sudo apt
endif

all: deps
	mkdir -p out
	ghc -O2 --make burningbar.hs -outputdir out -o burningbar

debug:
	ghci *.hs -DDEBUG

test:
	./runburningbar
	@echo using spec.bb, written Entities.swift \& Interface.swift

run:
	open ./TestGen.xcworkspace

lint:
	#cabal install hlint
	hlint *hs

deps:
	$(PM) install haskell-platform
	# or install manually from https://www.haskell.org/platform/
	cabal update
	cabal install base-unicode-symbols

clean:
	rm -rf out
	rm -rf *\.hi
	rm -rf *\.o
	rm -f Entities.swift Interface.swift
