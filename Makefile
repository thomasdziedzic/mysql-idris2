.PHONY: all install clean

all:
	idris2 --build mysql.ipkg

install:
	idris2 --install mysql.ipkg

clean:
	idris2 --clean mysql.ipkg
