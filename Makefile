default: 
	make -C src && mv src/gracec .

clean:
	make clean -C src

distclean:
	make distclean -C src && rm -rf gracec
