%option reentrant noyywrap bison-bridge bison-locations
%option extra-type="struct Extra *"

/* Подавление предупреждений для -Wall */
%option noinput nounput

%{

#include <stdio.h>
#include <stdlib.h>
#include "lexer.h"
#include "parser.tab.h"  /* файл генерируется Bison’ом с опцией -d */

#define YY_USER_ACTION \
  { \
    int i; \
    struct Extra *extra = yyextra; \
    if (! extra->continued ) { \
      yylloc->first_line = extra->cur_line; \
      yylloc->first_column = extra->cur_column; \
    } \
    extra->continued = false; \
    for (i = 0; i < yyleng; ++i) { \
      if (yytext[i] == '\n') { \
        extra->cur_line += 1; \
        extra->cur_column = 1; \
      } else { \
        extra->cur_column += 1; \
      } \
    } \
    yylloc->last_line = extra->cur_line; \
    yylloc->last_column = extra->cur_column; \
  }

void yyerror(YYLTYPE *loc, yyscan_t scanner, int level, const char *message) {
    printf("Error (%d,%d): %s\n", loc->first_line, loc->first_column, message);
}

%}

%%

[\r\t\n ]+

\(\*.*\*\) {
	printf("%s\n", yytext);
}

:=     return ASSIGN;
\+     return PLUS;
-      return MINUS;
\*     return MUL;
\/     return DIV_NUM;
\(     return LP;
\)     return RP;
,      return ZAP;
:      return DP;
;      return PZ;
IF     return IF;
WHILE  return WHILE;
\.     return POINT;
NOT    return NOT;
AND    return AND;
DIV    return DIV;
MOD    return MOD;
OR     return OR;
\<     return LT;
\>     return GT;
\<=    return LE;
\>=    return GE;
=      return EQ;
#      return NEQ;
ELSE   return ELSE;
THEN   return THEN;
DO     return DO;
NEW    return NEW;
\^     return KR;
"POINTER"[ \r\t\n]+"TO"  return POINT_TO;
BEGIN  return BEGIN_TOK;
VAR    return VAR; 
RECORD return RECORD;
TYPE   return TYPE; 
END    return END;

[0-9]+([\.0-9]+)? {
    yylval->val = strdup(yytext);
    return NUMBER;
}

[A-Za-z][A-Za-z0-9]* {
    yylval->val = strdup(yytext);
    return IDENT;
}


%%

void init_scanner(FILE *input, yyscan_t *scanner, struct Extra *extra) {
    extra->continued = false;
    extra->cur_line = 1;
    extra->cur_column = 1;

    yylex_init(scanner);
    yylex_init_extra(extra, scanner);
    yyset_in(input, *scanner);
}

void destroy_scanner(yyscan_t scanner) {
    yylex_destroy(scanner);
}
