run: build
	./propellor

dev: build tags

build: deps dist/setup-config
	if ! cabal build; then cabal configure; cabal build; fi
	ln -sf dist/build/propellor/propellor

deps:
	@if [ $$(whoami) = root ]; then apt-get -y install gnupg ghc cabal-install libghc-missingh-dev libghc-ansi-terminal-dev libghc-ifelse-dev libghc-unix-compat-dev libghc-hslogger-dev; fi || true

dist/setup-config: propellor.cabal
	cabal configure

clean:
	rm -rf dist Setup tags propellor privdata/local
	find -name \*.o -exec rm {} \;
	find -name \*.hi -exec rm {} \;

# hothasktags chokes on some template haskell etc, so ignore errors
tags:
	find . | grep -v /.git/ | grep -v /tmp/ | grep -v /dist/ | grep -v /doc/ | egrep '\.hs$$' | xargs hothasktags > tags 2>/dev/null

.PHONY: tags
