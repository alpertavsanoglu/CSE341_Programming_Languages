%{
#include <stdio.h>
#include <string.h>
#include "gpp.h"

extern int yylex();
extern int yyparse();
extern FILE *yyin;
void yyerror(const char *s);
int yylex();
SymbolTable table;
%}

%union{
    Valuef valuef;
    func_data ffunc;
    int bos;
    char id[50];
}


%start START
%token KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS KW_NIL KW_LIST
%token KW_APPEND KW_CONCAT KW_SET KW_DEF KW_FOR KW_IF
%token KW_EXIT KW_LOAD KW_DISPLAY KW_TRUE KW_FALSE
%token OP_PLUS OP_MINUS OP_DIV OP_MULT OP_OP OP_CP OP_COMMA 
%token COMMENT

%token <id>IDENTIFIER
%token <valuef> VALUEF

%type <bos> FUNCTION
%type <ffunc> EXP
%type <ffunc> FUNCEXP

%%

START: START EXIT
    | START INPUT
    | INPUT
    | EXIT
    ;

EXIT: OP_OP KW_EXIT OP_CP {return 0;}

INPUT: EXP { fprintf(yyout, "%df%d\n", $1.value.num, $1.value.denom);}
    | FUNCTION { printf("#function\n");}
    ;

EXP: OP_OP OP_PLUS EXP EXP OP_CP  { $$ = valuef_add($3, $4);}
    | OP_OP OP_MINUS EXP EXP OP_CP { $$ = valuef_sub($3, $4);}
    | OP_OP OP_MULT EXP EXP OP_CP { $$ = valuef_mult($3, $4);}
    | OP_OP OP_DIV EXP EXP OP_CP  { $$ = valuef_div($3, $4);}
    | OP_OP IDENTIFIER OP_CP{
       $$ = use_function(&table,$2,0,valuef_create(0,1),valuef_create(0,1));
    }
    | OP_OP IDENTIFIER EXP OP_CP{
       $$ = use_function(&table,$2,1,$3,valuef_create(0,1));
    }
    | OP_OP IDENTIFIER EXP EXP OP_CP{
       $$ = use_function(&table,$2,2,$3,$4);
    }
    | VALUEF {$$ = valuef_create($1.num,$1.denom);}
    ;
    
FUNCTION: OP_OP KW_DEF IDENTIFIER FUNCEXP OP_CP{
        define_function(&table,$3,0,$4);
        $$ = 0;
    }
    | OP_OP KW_DEF IDENTIFIER IDENTIFIER  FUNCEXP OP_CP{
        define_function(&table,$3,1,$5);
        $$ = 0;
    }
    | OP_OP KW_DEF IDENTIFIER IDENTIFIER IDENTIFIER  FUNCEXP OP_CP{
        define_function(&table,$3,2,$6);
        $$ = 0;
    }
    ;

FUNCEXP: OP_OP OP_PLUS IDENTIFIER IDENTIFIER OP_CP          { $$.func = &valuef_add;}
    | OP_OP OP_MINUS IDENTIFIER IDENTIFIER OP_CP            { $$.func = &valuef_sub;}
    | OP_OP OP_MULT IDENTIFIER IDENTIFIER OP_CP             { $$.func = &valuef_mult;}
    | OP_OP OP_DIV IDENTIFIER IDENTIFIER OP_CP              { $$.func = &valuef_div;}

%%

void yyerror(const char *s) {
    printf("SYNTAX ERROR!\n");
}

int main(int argc, char *argv[]) {
    init_table(&table);

    if(argc == 1){
        printf("Enter: \n");
        yyparse();
    }
    else if(argc == 2){
        FILE *fp;
        fp = fopen(argv[1], "r");
        yyin = fp;
        yyparse(); 
        fclose(fp);
    }
    else{
        printf("invalid number of arguments\n");
        return 0;
    }
    free_table(&table);

    return 0;
}
