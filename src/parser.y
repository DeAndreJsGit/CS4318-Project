%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<../src/tree.h>
#include<../src/strtab.h>

extern int yylineno;
extern int yylex(void);

int yywarning(char *msg);
int yyerror(char *msg);


typedef struct treenode tree;
extern tree *ast;

/* nodeTypes refer to different types of internal and external nodes
  that can be part of the abstract syntax tree.
  */
enum nodeTypes {PROGRAM, DECLLIST, DECL, VARDECL, TYPESPEC, FUNDECL,
                FORMALDECLLIST, FORMALDECL, FUNBODY, LOCALDECLLIST,
                STATEMENTLIST, STATEMENT, COMPOUNDSTMT, ASSIGNSTMT,
                CONDSTMT, LOOPSTMT, RETURNSTMT, EXPRESSION, RELOP,
                ADDEXPR, ADDOP, TERM, MULOP, FACTOR, FUNCCALLEXPR,
                ARGLIST, INTEGER, IDENTIFIER, VAR, ARRAYDECL, CHAR,
                FUNCTYPENAME};

enum opType {ADD, SUB, MUL, DIV, LT, LTE, EQ, GTE, GT, NEQ};

/* NOTE: mC has two kinds of scopes for variables : local and global.
  Variables declared outside any function are considered globals,
    whereas variables (and parameters) declared inside a function foo
    are local to foo.
  You should update the scope variable whenever you are inside a production
    that matches function definition (funDecl production).
  The rationale is that you are entering that function, so all variables,
    arrays, and other functions should be within this scope.
  You should pass this variable whenever you are
    calling the ST_insert or ST_lookup functions.
  This variable should be updated to scope = ""
    to indicate global scope whenever funDecl finishes.
  Treat these hints as helpful directions only.
  You may implement all of the functions as you like
    and not adhere to my instructions.
  As long as the directory structure is correct and the file names are correct,
    we are okay with it.
  */
char* scope = "";
%}

/* the union describes the fields available in the yylval variable */
%union
{
    int value;
    struct treenode *node;
    char *strval;
}

/*Add token declarations below.
  The type <value> indicates that the associated token will be
    of a value type such as integer, float etc.,
    and <strval> indicates that the associated token will be of string type.
  */
%token <strval> ID
%token <value> INTCONST
%token <value> CHARCONST

%token KWD_IF KWD_ELSE KWD_WHILE KWD_INT KWD_CHAR KWD_RETURN KWD_VOID
%token KWD_STRING

%token OPER_ADD OPER_SUB OPER_MUL OPER_DIV OPER_MOD
%token OPER_LT OPER_GT OPER_LTE OPER_GTE OPER_EQ OPER_NEQ
%token OPER_ASGN OPER_INC OPER_DEC OPER_AND OPER_OR OPER_NOT OPER_AT

%token LSQ_BRKT RSQ_BRKT LCRLY_BRKT RCRLY_BRKT LPAREN RPAREN
%token COMMA SEMICLN

%token STRCONST ERROR ILLEGAL_TOK

/* precedence to resolve dangling if else problem */
%nonassoc LOWER_THAN_ELSE
%nonassoc KWD_ELSE

%type <node> program declList decl varDecl typeSpecifier
%type <node> funDecl formalDeclList formalDecl funBody
%type <node> statementList statement compoundStmt assignStmt
%type <node> condStmt loopStmt returnStmt
%type <node> var expression relop addExpr addop term mulop factor
%type <node> funCallExpr argList

%start program

%%

program         : declList
                 {
                    tree *progNode = maketree(PROGRAM);
                    addChild(progNode, $1);
                    ast = progNode;
                    $$ = progNode;
                 }
                ;

declList        : singleDecl
                  {
                      tree *declListNode = maketree(DECLLIST);
                      addChild(declListNode, $1);
                      $$ = declListNode;
                  }
                | declList singleDecl
                  {
                      addChild($1, $2);
                      $$ = $1;
                  }
                ;

singleDecl      : varDecl
                  {
                      $$ = maketree(DECL);
                      addChild($$, $1);
                  }
                | funDecl
                  {
                      $$ = maketree(DECL);
                      addChild($$, $1);
                  }
                ;

varDecl         : typeSpecifier ID SEMICLN
                  {
                      tree *n  = maketree(VARDECL);
                      tree *id = maketreeWithVal(IDENTIFIER, 0);
                      id->strval = strdup($2);
                      addChild(n, $1);
                      addChild(n, id);
                      ST_insert($2, scope, $1->val, SCALAR);
                      $$ = n;
                  }
                | typeSpecifier ID LSQ_BRKT INTCONST RSQ_BRKT SEMICLN
                  {
                      tree *n    = maketree(VARDECL);
                      tree *id   = maketreeWithVal(IDENTIFIER, 0);
                      id->strval = strdup($2);
                      tree *size = maketreeWithVal(INTEGER, $4);
                      addChild(n, $1);
                      addChild(n, id);
                      addChild(n, size);
                      ST_insert($2, scope, $1->val, ARRAY);
                      $$ = n;
                  }
                ;

typeSpecifier   : KWD_INT   { $$ = maketreeWithVal(TYPESPEC, INT_TYPE);  }
                | KWD_CHAR  { $$ = maketreeWithVal(TYPESPEC, CHAR_TYPE); }
                | KWD_VOID  { $$ = maketreeWithVal(TYPESPEC, VOID_TYPE); }
                ;

funDecl         : typeSpecifier ID
                  {
                      scope = strdup($2);
                      ST_insert($2, "", $1->val, FUNCTION);
                  }
                  LPAREN formalDeclList RPAREN funBody
                  {
                      tree *n    = maketree(FUNDECL);
                      tree *ftn  = maketree(FUNCTYPENAME);
                      tree *id   = maketreeWithVal(IDENTIFIER, 0);
                      id->strval = strdup($2);
                      addChild(ftn, $1);
                      addChild(ftn, id);
                      addChild(n, ftn);
                      addChild(n, $5);
                      addChild(n, $7);
                      scope = "";
                      $$ = n;
                  }
                | typeSpecifier ID
                  {
                      scope = strdup($2);
                      ST_insert($2, "", $1->val, FUNCTION);
                  }
                  LPAREN RPAREN funBody
                  {
                      tree *n    = maketree(FUNDECL);
                      tree *ftn  = maketree(FUNCTYPENAME);
                      tree *id   = maketreeWithVal(IDENTIFIER, 0);
                      id->strval = strdup($2);
                      addChild(ftn, $1);
                      addChild(ftn, id);
                      addChild(n, ftn);
                      addChild(n, $6);
                      scope = "";
                      $$ = n;
                  }
                ;

formalDeclList  : formalDecl
                  {
                      tree *n = maketree(FORMALDECLLIST);
                      addChild(n, $1);
                      $$ = n;
                  }
                | formalDeclList COMMA formalDecl
                  {
                      addChild($1, $3);
                      $$ = $1;
                  }
                ;

formalDecl      : typeSpecifier ID
                  {
                      tree *n    = maketree(FORMALDECL);
                      tree *id   = maketreeWithVal(IDENTIFIER, 0);
                      id->strval = strdup($2);
                      addChild(n, $1);
                      addChild(n, id);
                      ST_insert($2, scope, $1->val, SCALAR);
                      $$ = n;
                  }
                | typeSpecifier ID LSQ_BRKT RSQ_BRKT
                  {
                      tree *n    = maketree(FORMALDECL);
                      tree *id   = maketreeWithVal(IDENTIFIER, 0);
                      id->strval = strdup($2);
                      addChild(n, $1);
                      addChild(n, id);
                      ST_insert($2, scope, $1->val, ARRAY);
                      $$ = n;
                  }
                ;

funBody         : LCRLY_BRKT statementList RCRLY_BRKT
                  {
                      tree *n = maketree(FUNBODY);
                      addChild(n, $2);
                      $$ = n;
                  }
                ;

statementList   : /* empty */
                  {
                      $$ = maketree(STATEMENTLIST);
                  }
                | statementList statement
                  {
                      addChild($1, $2);
                      $$ = $1;
                  }
                ;

statement       : SEMICLN        { $$ = maketree(STATEMENT); }
                | varDecl        { $$ = $1; }
                | compoundStmt   { $$ = $1; }
                | assignStmt     { $$ = $1; }
                | condStmt       { $$ = $1; }
                | loopStmt       { $$ = $1; }
                | returnStmt     { $$ = $1; }
                ;

compoundStmt    : LCRLY_BRKT statementList RCRLY_BRKT
                  {
                      tree *n = maketree(COMPOUNDSTMT);
                      addChild(n, $2);
                      $$ = n;
                  }
                ;

assignStmt      : var OPER_ASGN expression SEMICLN
                  {
                      tree *n = maketree(ASSIGNSTMT);
                      addChild(n, $1);
                      addChild(n, $3);
                      $$ = n;
                  }
                | expression SEMICLN
                  {
                      $$ = $1;
                  }
                ;

condStmt        : KWD_IF LPAREN expression RPAREN statement %prec LOWER_THAN_ELSE
                  {
                      tree *n = maketree(CONDSTMT);
                      addChild(n, $3);
                      addChild(n, $5);
                      $$ = n;
                  }
                | KWD_IF LPAREN expression RPAREN statement KWD_ELSE statement
                  {
                      tree *n = maketree(CONDSTMT);
                      addChild(n, $3);
                      addChild(n, $5);
                      addChild(n, $7);
                      $$ = n;
                  }
                ;

loopStmt        : KWD_WHILE LPAREN expression RPAREN statement
                  {
                      tree *n = maketree(LOOPSTMT);
                      addChild(n, $3);
                      addChild(n, $5);
                      $$ = n;
                  }
                ;

returnStmt      : KWD_RETURN SEMICLN
                  {
                      $$ = maketree(RETURNSTMT);
                  }
                | KWD_RETURN expression SEMICLN
                  {
                      tree *n = maketree(RETURNSTMT);
                      addChild(n, $2);
                      $$ = n;
                  }
                ;

var             : ID
                  {
                      tree *n    = maketree(VAR);
                      tree *id   = maketreeWithVal(IDENTIFIER, 0);
                      id->strval = strdup($1);
                      addChild(n, id);
                      if (ST_lookup($1, scope) < 0 && ST_lookup($1, "") < 0)
                          yywarning("undeclared variable");
                      $$ = n;
                  }
                | ID LSQ_BRKT addExpr RSQ_BRKT
                  {
                      tree *n    = maketree(VAR);
                      tree *id   = maketreeWithVal(IDENTIFIER, 0);
                      id->strval = strdup($1);
                      addChild(n, id);
                      addChild(n, $3);
                      if (ST_lookup($1, scope) < 0 && ST_lookup($1, "") < 0)
                          yywarning("undeclared variable");
                      $$ = n;
                  }
                ;

expression      : addExpr
                  {
                      $$ = $1;
                  }
                | expression relop addExpr
                  {
                      tree *n = maketree(EXPRESSION);
                      addChild(n, $1);
                      addChild(n, $2);
                      addChild(n, $3);
                      $$ = n;
                  }
                ;

relop           : OPER_LT  { $$ = maketreeWithVal(RELOP, LT);  }
                | OPER_LTE { $$ = maketreeWithVal(RELOP, LTE); }
                | OPER_EQ  { $$ = maketreeWithVal(RELOP, EQ);  }
                | OPER_GTE { $$ = maketreeWithVal(RELOP, GTE); }
                | OPER_GT  { $$ = maketreeWithVal(RELOP, GT);  }
                | OPER_NEQ { $$ = maketreeWithVal(RELOP, NEQ); }
                ;

addExpr         : term
                  {
                      $$ = $1;
                  }
                | addExpr addop term
                  {
                      tree *n = maketree(ADDEXPR);
                      addChild(n, $1);
                      addChild(n, $2);
                      addChild(n, $3);
                      $$ = n;
                  }
                ;

addop           : OPER_ADD { $$ = maketreeWithVal(ADDOP, ADD); }
                | OPER_SUB { $$ = maketreeWithVal(ADDOP, SUB); }
                ;

term            : factor
                  {
                      $$ = $1;
                  }
                | term mulop factor
                  {
                      tree *n = maketree(TERM);
                      addChild(n, $1);
                      addChild(n, $2);
                      addChild(n, $3);
                      $$ = n;
                  }
                ;

mulop           : OPER_MUL { $$ = maketreeWithVal(MULOP, MUL); }
                | OPER_DIV { $$ = maketreeWithVal(MULOP, DIV); }
                ;

factor          : LPAREN expression RPAREN { $$ = $2; }
                | var                      { $$ = $1; }
                | funCallExpr              { $$ = $1; }
                | INTCONST                 { $$ = maketreeWithVal(INTEGER, $1); }
                | CHARCONST                { $$ = maketreeWithVal(CHAR, $1);   }
                ;

funCallExpr     : ID LPAREN RPAREN
                  {
                      tree *n    = maketree(FUNCCALLEXPR);
                      tree *id   = maketreeWithVal(IDENTIFIER, 0);
                      id->strval = strdup($1);
                      addChild(n, id);
                      if (ST_lookup($1, "") < 0)
                          yywarning("undeclared function");
                      $$ = n;
                  }
                | ID LPAREN argList RPAREN
                  {
                      tree *n    = maketree(FUNCCALLEXPR);
                      tree *id   = maketreeWithVal(IDENTIFIER, 0);
                      id->strval = strdup($1);
                      addChild(n, id);
                      addChild(n, $3);
                      if (ST_lookup($1, "") < 0)
                          yywarning("undeclared function");
                      $$ = n;
                  }
                ;

argList         : expression
                  {
                      tree *n = maketree(ARGLIST);
                      addChild(n, $1);
                      $$ = n;
                  }
                | argList COMMA expression
                  {
                      addChild($1, $3);
                      $$ = $1;
                  }
                ;

%%

int yywarning(char *msg){
  printf("warning: line %d: %s\n", yylineno, msg);
  return 0;
}

int yyerror(char * msg){
  printf("error: line %d: %s\n", yylineno, msg);
  return 0;
}
