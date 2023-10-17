#ifndef __LEXER_H__
#define __LEXER_H__

extern int lineno;

int  yylex();
void yyerror(const char* msg);

#endif
