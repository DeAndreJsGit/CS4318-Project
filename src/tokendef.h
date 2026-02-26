#ifndef _TOKENDEF_H_
#define _TOKENDEF_H_

/* major stuff */
#define ID          251
#define INTCONST    252
#define CHARCONST   253
#define STRCONST    254

/* keywords */
#define KWD_IF      255
#define KWD_ELSE    256
#define KWD_WHILE   257
#define KWD_INT     258
#define KWD_STRING  259
#define KWD_CHAR    260
#define KWD_RETURN  261
#define KWD_VOID    262

/* operators */
#define OPER_ADD    263
#define OPER_SUB    264
#define OPER_MUL    265
#define OPER_DIV    266
#define OPER_LT     267
#define OPER_GT     268
#define OPER_GTE    269
#define OPER_LTE    270
#define OPER_EQ     271
#define OPER_NEQ    272
#define OPER_ASGN   273
#define OPER_AT     274
#define OPER_INC    275
#define OPER_DEC    276
#define OPER_AND    277
#define OPER_OR     278
#define OPER_NOT    279
#define OPER_MOD    280

/* brackets & parens */
#define LSQ_BRKT    281
#define RSQ_BRKT    282
#define LCRLY_BRKT  283
#define RCRLY_BRKT  284
#define LPAREN      285
#define RPAREN      286

/* punctuation */
#define COMMA       287
#define SEMICLN     288

/* other */
#define ERROR       289
#define ILLEGAL_TOK 290

#endif //ifndef _TOKENDEF_H_
