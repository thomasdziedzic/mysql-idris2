INSTALLDIR = `idris2 --libdir`/mysql/lib

.PHONY: all install clean

all:
	gcc -c -fPIC libidrismysql.c -o libidrismysql.o
	gcc libidrismysql.o -shared -o libidrismysql.so

install:
	@if ! [ -d $(INSTALLDIR) ]; then mkdir -p $(INSTALLDIR); fi
	install libidrismysql.so $(wildcard *.h) $(INSTALLDIR)

clean:
	rm -f libidrismysql.o libidrismysql.so
