CC = /usr/local/bin/gcc-6
CFLAGS = -Wall -pedantic
lisp: ul.o
	$(CC) $(CFLAGS) -o lisp ul.o

ul.o: ul.c
	$(CC) $(CFLAGS) -g -c ul.c

tests: lisp
	for t in `ls test_*.lisp`; do cat $$t && ./lisp < $$t; done

.PHONY: tests
