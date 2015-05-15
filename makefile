
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
	ghc -O2 --make src/BurningBar.hs -isrc -outputdir out -o burningbar

debug:
	ghci -isrc src/Language.hs src/BurningBar.hs src/Parse.hs src/Swift.hs -DDEBUG

example:
	./runburningbar
	@echo using spec.burnbar, written Entities.swift and Interface.swift.

test:
	cabal install --only-dependencies --enable-tests && cabal build && cabal test

run:
	open ./TestGen.xcworkspace

update:
	git pull -u
	cabal build

restart: update
	dist/build/burningbard/burningbard

lint:
	# cabal install hlint
	hlint *hs

deps:
	$(INSTALL_DEPS)
	# or install manually: https://www.haskell.org/platform/

clean:
	rm -rf out
	rm -rf *\.hi
	rm -rf *\.o
	rm -f Entities.swift Interface.swift
	cabal clean

