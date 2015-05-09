
CASK_VER := $(shell brew cask --version 2>/dev/null)

ifeq ($(shell uname -s), Linux)
INSTALL_DEPS = sudo apt install ghc cabal-install
else
ifneq ($(strip $(CASK_VER)),)
INSTALL_DEPS := brew cask install haskell-platform
else
INSTALL_DEPS := brew install ghc cabal-install
endif
endif



all: deps
	mkdir -p out
	ghc -O2 --make BurningBar.hs -outputdir out -o burningbar

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
	$(INSTALL_DEPS)
	# or install manually: https://www.haskell.org/platform/

clean:
	rm -rf out
	rm -rf *\.hi
	rm -rf *\.o
	rm -f Entities.swift Interface.swift
