%{

#include <stdlib.h>
#include <stdio.h>

char* strdup(char*) ;

#include "sexp.h"

SExp  XNIL = { NIL } ;
SExp* SX_NIL = &XNIL ;

SExp  XTRUE = { TRUE } ;
SExp* SX_TRUE = &XTRUE ;

SExp  XFALSE = { FALSE } ;
SExp* SX_FALSE = &XFALSE ;

int yylex(void) ;
void yyerror(char const*) ;

SExp* result = NULL ;

%}

%union {
  int   integer ;
  char* symbol ;
  union SExp* sexp ;
} 

%type <sexp> sexp sexpl

%token <integer> INT
%token <symbol>  SYM

%%

start : sexp { result = $1 ; }
      ;

sexp : INT { $$ = integer($1) ; } 
     | SYM { $$ = symbol($1) ; }
     | 't' { $$ = SX_TRUE ; }
     | 'f' { $$ = SX_FALSE ; }
     | '(' sexpl ')' { $$ = $2 ; }
     ;

sexpl : sexp sexpl { $$ = cons($1, $2) ; }
      |            { $$ = SX_NIL ; }
      ;


%%

void PrintSX(SExp*) ;
void PrintSXL(SExp*) ;


void PrintSX(SExp* sx) {
  switch (sx->tag) {
    case INTEGER:
      printf("%i", sx->integer.value) ;
    break ;

    case TRUE:
      printf("#t") ;
    break ;

    case FALSE:
      printf("#f") ;
    break ;

    case NIL:
      printf("()") ;
    break ;

    case SYMBOL:
      printf("%s", sx->symbol.name) ;
    break ;

    case CONS:
      printf("(") ;
      PrintSXL(sx) ;
      printf(")") ;
    break ;
  }
}

void PrintSXL(SExp* sxl) {
  SExp* car ;
  SExp* cdr ;

  switch (sxl->tag) {
    case NIL:
    break ;

    case CONS:
      car = sxl->cons.car ;
      cdr = sxl->cons.cdr ;
      PrintSX(car) ;
      if (cdr->tag != NIL)
        printf(" ") ;
      PrintSXL(cdr) ;
    break ;

    default:
      printf("error: cannot print improper list") ;
    break ;
  }
}



int main (int argc, char* argv[]) {

  int r ; 
 
  (void)argc; (void)argv; 

  r = yyparse() ;

  PrintSX(result) ;

  printf("\n") ;

  return EXIT_SUCCESS ;
}

void yyerror(char const* problem) {
  printf("problem: %s\n",problem) ;
  exit(-1) ;
}

