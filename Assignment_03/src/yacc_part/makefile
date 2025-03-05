


all: a.out

y.tab.c: gpp_interpreter.y gpp.h
	yacc gpp_interpreter.y

lex.yy.c: gpp_lexer.l y.tab.c
	flex gpp_lexer.l

gpp.o: gpp.c gpp.h
	gcc -c gpp.c -o gpp.o

a.out: gpp.o lex.yy.c y.tab.c
	gcc gpp.o lex.yy.c -o a.out

clean:
	rm -f *.out
	rm -f y.tab.c
	rm -f lex.yy.c
	rm -f gpp.o
