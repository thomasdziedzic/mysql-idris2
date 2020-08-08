.PHONY: all test install clean

all:
	idris2 --build mysql.ipkg
	idris2 --build mysql-test.ipkg

test:
	build/exec/mysql-test

install:
	idris2 --install mysql.ipkg

clean:
	idris2 --clean mysql.ipkg
	idris2 --clean mysql-test.ipkg
