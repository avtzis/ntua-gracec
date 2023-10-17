default: compiler library
	mv src/gracec lib/libgrc.a .

compiler:
	make -C src

library:
	make -C lib

clean:
	make clean -C src

distclean:
	make distclean -C src && rm -rf gracec
