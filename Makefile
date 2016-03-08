targets = lambda

.PHONY : all clean distclean $(targets)

all : $(targets)

.cabal-sandbox cabal.sandbox.config :
	cabal sandbox init
	cabal install -j parsec

lambda : .cabal-sandbox cabal.sandbox.config
	cabal exec -- ghc --make -j$(shell nproc) -Wall -fno-warn-name-shadowing -outputdir tmp -o lambda -main-is Simple.main Simple.lhs
	strip lambda

clean :
	rm -rf .cabal-sandbox/ cabal.sandbox.config tmp

distclean : clean
	rm -f $(targets)
	which git >/dev/null && git clean -xnd
