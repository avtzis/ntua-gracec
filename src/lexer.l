%{
#include <cstring>
#include "ast.hpp"
#include "lexer.hpp"
#include "parser.hpp"

#define T_eof 0

int lineno = 1;
%}

L [A-Za-z]
D [0-9]
W [ \t\r]

%x SINGLE_COMMENT
%x MULTI_COMMENT

%option noyywrap

%%

"char"    { return T_char;    }
"do"      { return T_do;      }
"else"    { return T_else;    }
"fun"     { return T_fun;     }
"if"      { return T_if;      }
"int"     { return T_int;     }
"nothing" { return T_nothing; }
"ref"     { return T_ref;     }
"return"  { return T_return;  }
"then"    { return T_then;    }
"var"     { return T_var;     }
"while"   { return T_while;   }

"not"     { yylval.op = strdup(yytext); return T_not;     }
"mod"     { yylval.op = strdup(yytext); return T_mod;     }
"and"     { yylval.op = strdup(yytext); return T_and;     }
"or"      { yylval.op = strdup(yytext); return T_or;      }
"div"     { yylval.op = strdup(yytext); return T_div;     }
"<="      { yylval.op = strdup(yytext); return T_lte;     }
">="      { yylval.op = strdup(yytext); return T_gte;     }

"<-"                                { return T_assign;  }
[\+\-\*\=\#\<\>\(\)\[\]\{\}\,\;\:]  { yylval.op = strdup(yytext); return yytext[0]; }

{L}({L}|{D}|_)*       { yylval.var = strdup(yytext); return T_id;      }
{D}+                  { yylval.num = atoi(yytext); return T_const;   }

'(\\[ntr0\\\'\"]|\\x({D}|[A-Fa-f]){2}|[^\\\n])'   { yylval.character = strndup(yytext+1, strlen(yytext)-2); return T_character; }
\"(([^\"\n]*("\\\"")*[^\"\n]*)*)\"                { yylval.str = strndup(yytext+1, strlen(yytext)-2); return T_string;    }

\n      { ++lineno;     }
{W}+    { /* nothing */ }

<INITIAL>{
"$$"      BEGIN(MULTI_COMMENT);
"$"       BEGIN(SINGLE_COMMENT);
}

<MULTI_COMMENT>{
"$$"      BEGIN(INITIAL);
[^\$\n]+   // eat comment in chunks
"$"        // eat the lone dollar
\n        ++lineno;
}

<SINGLE_COMMENT>{
\n          ++lineno, BEGIN(INITIAL);
[^\n]+      // eat comment
}

.       { yyerror("Illegal token"); }

%%
