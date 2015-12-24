/* S-Expressions. */

union SExp ;

typedef union SExp SExp ;

typedef enum { FALSE = 0, TRUE = 1, NIL, CONS, SYMBOL, INTEGER } SExpType ;

union SExp {
  SExpType tag ;

  struct {
    SExpType tag ;
    SExp* car ;
    SExp* cdr ;
  } cons ;

  struct {
    SExpType tag ;
    char* name ;
  } symbol ;

  struct {
    SExpType tag ;
    int value ;
  } integer ;
} ;



static SExp* new_SExp(SExpType t) {
  SExp* r = malloc(sizeof(SExp)) ;
  r->tag = t ;
  return r ;
}

static SExp* cons(SExp* car, SExp* cdr) {
  SExp* r = new_SExp(CONS) ;
  r->cons.car = car ;
  r->cons.cdr = cdr ;
  return r ;
}

static SExp* symbol(char* name) {
  SExp* r = new_SExp(SYMBOL) ;
  r->symbol.name = strdup(name) ;
  return r ;
}

static SExp* integer(int value) {
  SExp* r = new_SExp(INTEGER) ;
  r->integer.value = value ;
  return r; 
}

