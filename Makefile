build: dist/setup-config
	cabal build
	$(MAKE) tags

dist/setup-config:
	cabal configure

clean:
	rm -rf dist Setup tags
	find -name \*.o -exec rm {} \;
	find -name \*.hi -exec rm {} \;

# hothasktags chokes on some template haskell etc, so ignore errors
tags:
	find . | grep -v /.git/ | grep -v /tmp/ | grep -v /dist/ | grep -v /doc/ | egrep '\.hs$$' | xargs hothasktags > tags 2>/dev/null
