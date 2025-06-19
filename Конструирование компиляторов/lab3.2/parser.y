%{
#include <stdio.h>
#include "lexer.h"
%}

%define api.pure
%locations

/* параметр для yylex() */
%lex-param {yyscan_t scanner}

/* параметры для yyparse() */
%parse-param {yyscan_t scanner}
%parse-param {int level}

%union {
    char* val;
}

%right ASSIGN

/* токены можно записывать как символическими именами, так и символами ASCII */
%left PLUS MINUS OR
%left MUL DIV AND DIV_NUM MOD
%left LT GT LE GE EQ NEQ
%left NOT

%token POINT PZ DP ELSE WHILE THEN IF END DO LP RP NEW KR POINT_TO BEGIN_TOK VAR ZAP RECORD TYPE

%token <val> IDENT
%token <val> NUMBER

%start Prog

%{
int yylex(YYSTYPE *yylval_param, YYLTYPE *yylloc_param, yyscan_t scanner);
void yyerror(YYLTYPE *loc, yyscan_t scanner, int level, const char *message);
void tabs(int level) {
	for (int i = 0; i < level; i++) {
		printf("  ");
	}
}
%}

%%

Prog:
      Types Vars Body
    ;

Types:
	  TYPE {printf("\nTYPE\n"); level = 1;} Rows_types
	;

Rows_types:
	  Rows_types Type
	| Type 
	;
	
Type:
	  IDENT {tabs(level); printf("%s", $IDENT);} EQ {printf(" = ");} 
	  	Record Rows END PZ {level--; tabs(level); printf("END;\n");} 
	;
	
Record:
	  RECORD {printf("RECORD\n"); level++;}
	| RECORD LP IDENT RP {printf("RECORD(%s)\n", $IDENT); level++;}
	;
	
Rows:
	  Rows Row 
	| Row
	;

Row:
	  Vars_dec DP {printf(" : ");} Type_dec PZ {printf(";\n");}
	;
	
Type_dec:
	  POINT_TO IDENT {printf("POINTER TO %s", $IDENT);}
	| IDENT {printf("%s", $IDENT);}
	;
	
Vars_dec:
	  Vars_dec ZAP {printf(", ");} IDENT {printf("%s", $IDENT);}
	| IDENT {tabs(level); printf("%s", $IDENT);}
	;
	
Vars:
	  VAR {printf("\nVAR\n"); level = 1;} Rows_vars
	;
	
Rows_vars:
	  Rows_vars Row_var
	| Row_var
	;
	
Row_var:
	  Vars_dec DP {printf(" : ");} Type_dec PZ {printf(";\n");}
	;
	
Body:
	  BEGIN_TOK {printf("\nBEGIN\n"); level = 1;} Stmts END POINT {printf("END.\n");}
	;
	
Stmts:
	  Stmts Stmt
	| Stmt
	;
	
Stmt:
	  Assign
	| New
	| While
	| If
	;
	
Assign:
	  {tabs(level);} Left ASSIGN {printf(" := ");} Right PZ {printf(";\n");}
	;
	
Left:
	  IDENT {printf("%s", $IDENT);}
	| Field
	| IDENT KR {printf("%s^", $IDENT);}
	| Field KR {printf("^");}
	;
	
Field:
	  Field POINT IDENT {printf(".%s", $IDENT);}
	| IDENT POINT IDENT {printf("%s.%s", $1, $3);}
	;
	
Right:
	  Expr
	;
	
Expr:
	  IDENT {printf("%s", $IDENT);}
	| IDENT KR {printf("%s^", $IDENT);}
	| NUMBER {printf("%s", $NUMBER);}
	| Expr BinOp Expr
	| Field
	| Field KR {printf("^");}
	;
	
New:
	  NEW LP IDENT RP {tabs(level); printf("NEW(%s)", $IDENT);} PZ {printf(";\n");}
	;
	
While:
	  WHILE {tabs(level); printf("WHILE ");} Cond DO {printf(" DO\n"); level++;} 
	  	Stmts END PZ {level--; tabs(level); printf("END;\n");}
	;
	
Cond:
	  Expr CmpOp Expr
	| NOT {printf("NOT ");} Cond
	| Cond BoolOp Cond
	;	

If:
	  IF {tabs(level); printf("IF ");} Cond THEN {printf(" THEN\n"); level++;} 
	  	Stmts ELSE {tabs(level-1); printf("ELSE\n");} Stmts END PZ {level--; tabs(level); printf("END;\n");}
	;
	
BinOp:
	  MUL     {printf(" * ");}
	| DIV     {printf(" DIV ");}
	| DIV_NUM {printf(" / ");}
	| MOD     {printf(" MOD ");}
	| PLUS    {printf(" + ");}
	| MINUS   {printf(" - ");}
	;
	
CmpOp:
	  LT  {printf(" < ");}
	| GT  {printf(" > ");}
	| LE  {printf(" <= ");}
	| GE  {printf(" >= ");}
	| EQ  {printf(" = ");}
	| NEQ {printf(" # ");}
	;
	
BoolOp:
	  AND {printf(" AND ");}
	| OR  {printf(" OR ");}
	;
	

%%

int main(int argc, char *argv[]) {
    FILE *input = 0;
    yyscan_t scanner;
    int level = 0;
    struct Extra extra;

    if (argc > 1) {
        printf("Read file %s\n", argv[1]);
        input = fopen(argv[1], "r");
    } else {
        printf("No file in command line, use stdin\n");
        input = stdin;
    }

    init_scanner(input, &scanner, &extra);
    yyparse(scanner, level);
    destroy_scanner(scanner);

    if (input != stdin) {
        fclose(input);
    }

    return 0;
}
