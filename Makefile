CABAL?=cabal

run: deps build
	./propellor

dev: build tags

build: dist/setup-config
	if ! $(CABAL) build; then $(CABAL) configure; $(CABAL) build; fi
	ln -sf dist/build/config/config propellor

deps:
	@if [ $$(whoami) = root ]; then apt-get --no-upgrade --no-install-recommends -y install gnupg ghc cabal-install libghc-missingh-dev libghc-ansi-terminal-dev libghc-ifelse-dev libghc-unix-compat-dev libghc-hslogger-dev libghc-network-dev libghc-quickcheck2-dev libghc-mtl-dev libghc-monadcatchio-transformers-dev; fi || true
	@if [ $$(whoami) = root ]; then apt-get --no-upgrade --no-install-recommends -y install libghc-async-dev || (cabal update; cabal install async); fi || true

dist/setup-config: propellor.cabal
	if [ "$(CABAL)" = ./Setup ]; then ghc --make Setup; fi
	$(CABAL) configure

install:
	install -d $(DESTDIR)/usr/bin $(DESTDIR)/usr/src/propellor
	install -s dist/build/wrapper/wrapper $(DESTDIR)/usr/bin/propellor
	$(CABAL) sdist
	cat dist/propellor-*.tar.gz | \
		(cd $(DESTDIR)/usr/src/propellor && tar zx --strip-components=1)

clean:
	rm -rf dist Setup tags propellor propellor-wrapper privdata/local
	find -name \*.o -exec rm {} \;
	find -name \*.hi -exec rm {} \;

# hothasktags chokes on some template haskell etc, so ignore errors
tags:
	find . | grep -v /.git/ | grep -v /tmp/ | grep -v /dist/ | grep -v /doc/ | egrep '\.hs$$' | xargs hothasktags > tags 2>/dev/null

# Upload to hackage.
hackage:
	@cabal sdist
	@cabal upload dist/*.tar.gz

.PHONY: tags
