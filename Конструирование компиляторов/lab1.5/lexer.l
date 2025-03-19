%option noyywrap bison-bridge bison-locations
%{
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>


#define TAG_IDENT 1
#define TAG_NUMBER 2
#define TAG_MINUS 3
#define TAG_LOWER 4
#define TAG_LOWER_OR_EQ 5
#define TAG_KEYWORD 6


char *tag_names[] =
{
	"END_OF_PROGRAM", "IDENT" , "NUMBER",
	"MINUS", "LOWER", "LOWER_OR_EQ", "KEYWORD"
};

struct Position
{
	int line, pos, index;
};

struct ErrList
{
	char mes[255];
	struct ErrList* next;
};

typedef struct {
    char** names;
    int size;
    int capacity;
} Dictionary;

void init_dict(Dictionary* dict) {
    dict->size = 0;
    dict->capacity = 10;
    dict->names = (char**)malloc(dict->capacity * sizeof(char*));
}

void free_dict(Dictionary* dict) {
    for (int i = 0; i < dict->size; i++) {
        free(dict->names[i]);
    }
    free(dict->names);
    dict->size = 0;
    dict->capacity = 0;
}

int add_name(Dictionary* dict, char* name) {
    for (int i = 0; i < dict->size; i++) {
        if (strcmp(dict->names[i], name) == 0) {
            return i;
        }
    }

    if (dict->size >= dict->capacity) {
        dict->capacity *= 2;
        dict->names = (char**)realloc(dict->names, dict->capacity * sizeof(char*));
    }

    dict->names[dict->size] = strdup(name);
    int code = dict->size;
    dict->size++;
    return code;
}

char* get_name(Dictionary* dict, int index) {
    if (index < 0 || index >= dict->size) {
        return NULL;
    }
    return dict->names[index];
}

void print_pos(struct Position *p)
{
	printf("(%d,%d)", p->line, p->pos);
}

struct Fragment
{
	struct Position starting, following;
};

typedef struct Fragment YYLTYPE;

void print_frag (struct Fragment *f)
{
	print_pos(&(f->starting));
	printf("-");
	print_pos(&(f->following));
}

union Token
{
	int code;
	long num;
	char* val;
};

typedef union Token YYSTYPE;

int continued;
struct Position cur;
struct ErrList* error_list = NULL;
Dictionary names;

# define YY_USER_ACTION \
	{ \
		int i; \
		if (! continued ) \
			yylloc -> starting = cur ; \
		continued = 0; \
		for (i = 0; i < yyleng ; i ++) \
		{ \
			if ( yytext [i] == '\n') \
			{ \
				cur.line ++; \
				cur.pos = 1; \
			} \
			else \
				cur.pos ++; \
			cur.index ++; \
		} \
		yylloc -> following = cur ; \
	}
	
void init_scanner(FILE *f)
{
	continued = 0;
	cur.line = 1;
	cur.pos = 1;
	cur.index = 0;
	yyin = f;
}

void err(char *msg)
{
	struct ErrList* new_err = (struct ErrList*)malloc(sizeof(struct ErrList));
	
	snprintf(new_err->mes, 255, "Error (%d, %d): %s", cur.line, cur.pos, msg);
	
    new_err->next = error_list; 
    error_list = new_err;
}

void free_errors()
{
	while (error_list != NULL) 
	{
		struct ErrList* err;
		err = error_list;
		error_list = error_list->next;
		free(err);
	}
}
%}

LETTER [a-zA-Z]
LETTER_VOWEL [aoiueyAOIUEY]
DIGIT [0-9]
IDENT {LETTER_VOWEL}({LETTER})*
NUMBER (-)?{DIGIT}+
KEYWORD ((o|O)(p|P)(e|E)(n|N)|(c|C)(l|L)(o|O)(s|S)(e|E))

%x COMMENTS CHAR_1 CHAR_2	



%%
[\n \t]+
					
\< 						return TAG_LOWER;
\<\= 					return TAG_LOWER_OR_EQ;
--						return TAG_MINUS;


{KEYWORD}				{
							char* key_name = (char*)malloc(strlen(yytext) + 1);
    						strcpy(key_name, yytext);
    						
    						int len = strlen(key_name);
							for(int i = 0; i<len; i++){
							  	key_name[i] = tolower(key_name[i]);
							}
							
							yylval->val = key_name;
							return TAG_KEYWORD;
						}	
						
{IDENT} 				{
							char ident_name[strlen(yytext) + 1];
    						strcpy(ident_name, yytext);
							int len = strlen(ident_name);
							for(int i = 0; i<len; i++){
							  	ident_name[i] = tolower(ident_name[i]);
							}
							
							int code = add_name(&names, ident_name);
							yylval->code = code;
							return TAG_IDENT;
						}
						
{NUMBER} 				{
							yylval->num = atoi(yytext);
							return TAG_NUMBER;
						}	
						
. 						err ("unexpected character");			



%%

int main()
{	
	FILE *f = fopen("text.txt", "r");
	
	int tag;
	YYSTYPE value;
	YYLTYPE coords;
	init_scanner(f);
	init_dict(&names);
	do
	{
		tag = yylex(&value, &coords);
		if (tag != 0)
		{
			switch(tag)
			{
				case TAG_IDENT:
					printf("%s (%d, %d)-(%d, %d): %s\n", tag_names[TAG_IDENT], 
						coords.starting.line, coords.starting.pos, 
						coords.following.line, coords.following.pos, get_name(&names, value.code));
					break;
				case TAG_KEYWORD:
					printf("%s (%d, %d)-(%d, %d): %s\n", tag_names[TAG_KEYWORD],
				    	coords.starting.line, coords.starting.pos, 
						coords.following.line, coords.following.pos, value.val);
					free(value.val);
					break;
				case TAG_NUMBER:
					printf("%s (%d, %d)-(%d, %d): %ld\n", tag_names[TAG_NUMBER], 
						coords.starting.line, coords.starting.pos, 
						coords.following.line, coords.following.pos, value.num);
					break;
				case TAG_MINUS:
					printf("%s (%d, %d)-(%d, %d)\n", tag_names[TAG_MINUS], 
						coords.starting.line, coords.starting.pos, 
						coords.following.line, coords.following.pos);
					break;
				case TAG_LOWER:
					printf("%s (%d, %d)-(%d, %d)\n", tag_names[TAG_LOWER], 
						coords.starting.line, coords.starting.pos, 
						coords.following.line, coords.following.pos);
					break;
				case TAG_LOWER_OR_EQ:
					printf("%s (%d, %d)-(%d, %d)\n", tag_names[TAG_LOWER_OR_EQ], coords.starting.line, coords.starting.pos, 
						coords.following.line, coords.following.pos);
					break;
			}
		}	
	}
	while (tag != 0);

	while (error_list != NULL)
	{
		printf("%s\n", error_list->mes);
		error_list = error_list->next;
	}
	free_errors();
	free_dict(&names);
	fclose(f);
	return 0;
}
