
/* definitions */

%{
#include <stdio.h>
#include <stdlib.h>
#include "tokendef.h"

 /* integers to track line and column numbers */
int scancol = 1;
int yycol = 1;
int scanlineno = 1;

 /* char pointer to pass error messages to driver.c */
char *yyerror;

 /* functions to update line and column numbers */
void updateCol();
void countLines();

 /* function to process a string literal */
int processString();

%}

newline         \n
whitespace      [ \t\r]+
integer         [1-9][0-9]*|0
integerlead0    0[0-9]+
character       '.'

 /* String can contain any characters between the double quotes */
 /* other than a newline or unescaped double quotes             */
string     \"([^\"\n\\]|\\.)*\"

 /* If the end quote is not found in the same line the string is unterminated */
untermstring    \"([^\"\n\\]|\\.)*\n|\"([^\"\n\\]|\\.)*$

comment         "/*"([^*]|\*+[^*/])*\*+"/"
multlncomment   "/*"([^*]|\*+[^*/])*\*+"/"
untermcomment   "/*"([^*]|\*+[^*/])*$

identifier      [a-zA-Z][a-zA-Z0-9]*
illidentifier   [0-9]+[a-zA-Z][a-zA-Z0-9]*

%%

 /* rules */

 /* 1. Keywords - must come before identifier rule */
"if"            { updateCol(); return KWD_IF; }
"else"          { updateCol(); return KWD_ELSE; }
"while"         { updateCol(); return KWD_WHILE; }
"int"           { updateCol(); return KWD_INT; }
"string"        { updateCol(); return KWD_STRING; }
"char"          { updateCol(); return KWD_CHAR; }
"return"        { updateCol(); return KWD_RETURN; }
"void"          { updateCol(); return KWD_VOID; }

 /* 2. Operators - multi-char first, then single-char */
"<="            { updateCol(); return OPER_LTE; }
">="            { updateCol(); return OPER_GTE; }
"=="            { updateCol(); return OPER_EQ; }
"!="            { updateCol(); return OPER_NEQ; }
"++"            { updateCol(); return OPER_INC; }
"--"            { updateCol(); return OPER_DEC; }
"&&"            { updateCol(); return OPER_AND; }
"||"            { updateCol(); return OPER_OR; }
"+"             { updateCol(); return OPER_ADD; }
"-"             { updateCol(); return OPER_SUB; }
"*"             { updateCol(); return OPER_MUL; }
"/"             { updateCol(); return OPER_DIV; }
"%"             { updateCol(); return OPER_MOD; }
"<"             { updateCol(); return OPER_LT; }
">"             { updateCol(); return OPER_GT; }
"="             { updateCol(); return OPER_ASGN; }
"!"             { updateCol(); return OPER_NOT; }
"@"             { updateCol(); return OPER_AT; }

 /* Suggestion: - Brackets and punctuation - ( ) [ ] { } ; must be above the catch-all or they will be marked as illegal tokens */
"["             { updateCol(); return LSQ_BRKT; }
"]"             { updateCol(); return RSQ_BRKT; }
"{"             { updateCol(); return LCRLY_BRKT; }
"}"             { updateCol(); return RCRLY_BRKT; }
"("             { updateCol(); return LPAREN; }
")"             { updateCol(); return RPAREN; }
","             { updateCol(); return COMMA; }
";"             { updateCol(); return SEMICLN; }

 /* 3. Identifiers - age, total */
{identifier}    { updateCol(); return ID; }
{illidentifier} { updateCol(); return ILLEGAL_TOK; }

 /* 4. Constants - 1, 'a', "hello" */
{integer}       { updateCol(); return INTCONST; }
{integerlead0}  { updateCol(); yyerror = "Integer cannot have leading zeros"; return ERROR; }
{character}     { updateCol(); return CHARCONST; }
{string}        { updateCol(); return processString(); }
{untermstring}  { updateCol(); yyerror = "Unterminated string"; return ERROR; }

 /* 5. Comments - countLines handles both single and multi-line */
{comment}       { countLines(); /* silently discard */ }
{untermcomment} { countLines(); yyerror = "Unterminated comment"; return ERROR; }

 /* 6. Other */
{newline}       { countLines(); }
{whitespace}    { updateCol(); }
.               { updateCol(); return ILLEGAL_TOK; }

%%

/* user routines */

/* TODO:
Implement this function to keep track of column numbers
*/

void updateCol() {
    yycol = scancol;
    scancol += yyleng;
}

/* TODO:
Implement this function to keep track of line numbers
*/
void countLines(){
    yycol = scancol;
    yylineno = scanlineno;
    for (int i = 0; i < yyleng; i++){
        if (yytext[i] == \n){
            scanlineno++;
            scancol = 1;
        }
        else scancol++;
    }
}


/* TODO:
Implement this function to
check for illegal escape sequences in string literals
and
convert valid escape sequences into escaped characters
(string[0] == '\\' and string[1] == 'n', then string[0] should be '\n')
*/
int processString(){
}