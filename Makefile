targets = lambda

.PHONY : all clean distclean $(targets)

all : $(targets)

.cabal-sandbox/ cabal.sandbox.config : lambda.cabal
	cabal sandbox init
	cabal install -j --only-dependencies

dist : .cabal-sandbox/ cabal.sandbox.config
	cabal configure

lambda : dist .cabal-sandbox/ cabal.sandbox.config
	cabal build -j
	strip -o lambda dist/build/lambda/lambda

clean :
	rm -rf dist/

distclean : clean
	rm -rf $(targets) .cabal-sandbox/ cabal.sandbox.config
	which git >/dev/null && git clean -xnd
