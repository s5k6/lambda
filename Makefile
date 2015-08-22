targets = lambda

.PHONY : all clean $(targets)

all : $(targets)

lambda :
	touch CompileTime.lhs
	ghc --make -j$(shell nproc) -Wall -fno-warn-name-shadowing -outputdir tmp -o lambda -main-is Simple.main Simple.lhs
	strip lambda

devel :
	ghc --make -j$(shell nproc) -Wall -fno-warn-name-shadowing -outputdir tmp -o lambda -main-is Simple.main Simple.lhs

devel-loop :
	while $(MAKE) devel && ./lambda; do sleep 0.1; clear; done

clean :
	which git >/dev/null && git clean -Xd -f || rm -rf tmp $(targets)
