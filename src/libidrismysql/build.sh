gcc -c -fPIC libidrismysql.c -o libidrismysql.o
gcc libidrismysql.o -shared -o ../libidrismysql.so
