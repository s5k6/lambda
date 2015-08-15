targets = lambda

.PHONY : all clean $(targets)

all : $(targets)

lambda :
	touch CompileTime.lhs
	ghc --make -j$(shell nproc) -Wall -fno-warn-name-shadowing -outputdir tmp -o lambda -main-is Simple.main Simple.lhs
	strip lambda

clean :
	which git >/dev/null && git clean -Xd -f || rm -rf tmp $(targets)
