.PHONY: all install clean

all:
	gcc -c -fPIC libidrismysql.c -o libidrismysql.o
	gcc libidrismysql.o -shared -o ../libidrismysql.so

install:

clean:
	rm -f libidrismysql.o ../libidrismysql.so
