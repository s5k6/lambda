targets = lambda

.PHONY : all clean $(targets)

all : $(targets)

lambda :
	ghc --make -Wall -fno-warn-name-shadowing -outputdir tmp -o lambda -main-is Simple.main Simple.lhs

clean :
	rm -rf tmp $(targets)
