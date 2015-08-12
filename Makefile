targets = lambda

.PHONY : all clean $(targets)

all : $(targets)

lambda :
	touch Plaintext.lhs
	ghc --make -j$(shell nproc) -Wall -fno-warn-name-shadowing -outputdir tmp -o lambda -main-is Simple.main Simple.lhs

clean :
	rm -rf tmp $(targets)
