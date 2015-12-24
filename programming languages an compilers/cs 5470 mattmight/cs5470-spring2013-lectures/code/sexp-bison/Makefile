default: sexp

sexp.tab.c sexp.tab.h: sexp.y
	bison -d sexp.y

slex.yy.c: slex.l sexp.tab.h
	flex -o slex.yy.c slex.l

sexp: slex.yy.c sexp.tab.c
	gcc -o sexp -ansi -W -Wall slex.yy.c sexp.tab.c

run: sexp
	./sexp

clean:
	rm -v -f sexp sexp.tab.h sexp.tab.c slex.yy.c tests/*.out

test: sexp
	for i in tests/*.py; do ./sexp < $$i > $$i.out; diff $$i.out $$i.expected; done
