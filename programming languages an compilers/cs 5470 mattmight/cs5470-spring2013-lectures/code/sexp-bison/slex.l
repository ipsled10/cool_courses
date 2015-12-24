%{

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "sexp.tab.h"

#include "sexp.h" 


%}

%option noyywrap

nl \n|\r|\r\n

ws [ \t\r\n]

id [^ #();,'`\t\r\n][^ ();,'`\t\r\n]*

nzd      [1-9]
digit    [0-9]

decint {nzd}{digit}*|0+

%%

<INITIAL>{ws}         {}
<INITIAL>"#f"         {return 'f';}
<INITIAL>"#t"         {return 't';}
<INITIAL>"("          {return '(';}
<INITIAL>")"          {return ')';}
<INITIAL>{decint}     {yylval.integer = atoi(yytext); return INT;}
<INITIAL>{id}         {yylval.symbol = strdup(yytext); return SYM;}
<INITIAL><<EOF>>      {return 0;}

.  {printf("error: unknown char in input: '%c'\n",*yytext); exit(-1);}

%%


