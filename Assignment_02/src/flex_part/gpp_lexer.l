%{
int temp = 0;
%}

comment	;;.*

%%
[ \t\r\n]+
and|or|not|equal|less|nil|list|append|concat|set|deffun|for|if|exit|load|print|true|false	{	printf("%s: KW_", yytext); 
													int i = 0;
    													while (yytext[i]) {
        													if (yytext[i] >= 'a' && yytext[i] <= 'z') {
            													yytext[i] = yytext[i] & ~32; // Convert lowercase to uppercase using bitwise operation
        													}
        												i++;
    													}
												printf("%s", yytext); printf("\n");					
												}
"+"												{	printf("%s: OP_PLUS\n", yytext);}
"-"												{	printf("%s: OP_MINUS\n", yytext);}
"/"												{	printf("%s: OP_DIV\n", yytext);}
"*"												{	printf("%s: OP_MULT\n", yytext);}
"("												{	printf("%s: OP_OP\n", yytext);}
")"												{	printf("%s: OP_CP\n", yytext);}
","												{	printf("%s: OP_COMMA\n", yytext);}
{comment}				{	for (int i = 0; yytext[i] != 0; i++) if (yytext[i] == '\r') yytext[i] = 0; printf("%s: COMMENT\n", yytext);}
([a-zA-Z])([0-9]|[a-zA-Z])*		{	printf("%s: IDENTIFIER\n", yytext);}
0|[1-9][0-9]*				{	printf("%s: VALUEI\n", yytext);}													//extra				
[1-9][0-9]*+f[0]([0-9]*)*		{	printf("%s: SYNTAX_ERROR (Divisor cant start with 0)\n", yytext); return 0;}	//for division check
0+f[1-9]([0-9]*)*			{	printf("%s: VALUEF\n", yytext);}						//for 0/digit division
[1-9][0-9]*+f[1-9]([0-9]*)*		{	printf("%s: VALUEF\n", yytext);}						//for fraction eg 12f21
\"[^\"\n]*\"   				{ for (int i = 0; yytext[i] != 0; i++) if (yytext[i + 1] == 0) yytext[i] = 0; printf("\": OP_OC\n%s: VALUESTR\n\": OP_CC\n", &yytext[1]);}	//extra
([a-zA-Z])([0-9]|[a-zA-Z]|_)*		{	printf("%s: IDENTIFIER WITH _\n", yytext);}												//extra


0[0-9]*								{	printf("%s: SYNTAX_ERROR (Integer cant start with 0)\n", yytext); return 0;}
0|[1-9][0-9]*[a-zA-Z]						{	printf("SYNTAX_ERROR %s cannot be tokenized (Identifier cant start with digit)\n", yytext); return 0;}
.								{	printf("LEXICAL ERROR %s : Cannot be tokenized (Not defined in G++ Language)\n",yytext); return 0;}	
[0-9]*+[a-zA-Z]							{	printf("%s: SYNTAX_ERROR (Identifier cant start with digit)\n", yytext); return 0;}
<<EOF>>								{	return 0;	}


%%

int yywrap(){}
int main(int argc, char ** argv) {
	if (argc > 1) yyin = fopen(argv[1], "r");
	yylex();
	return 0;
}
