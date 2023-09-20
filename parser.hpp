/* A Bison parser, made by GNU Bison 3.8.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_PARSER_HPP_INCLUDED
# define YY_YY_PARSER_HPP_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    T_char = 258,                  /* "char"  */
    T_do = 259,                    /* "do"  */
    T_else = 260,                  /* "else"  */
    T_fun = 261,                   /* "fun"  */
    T_if = 262,                    /* "if"  */
    T_int = 263,                   /* "int"  */
    T_nothing = 264,               /* "nothing"  */
    T_ref = 265,                   /* "ref"  */
    T_return = 266,                /* "return"  */
    T_then = 267,                  /* "then"  */
    T_var = 268,                   /* "var"  */
    T_while = 269,                 /* "while"  */
    T_assign = 270,                /* "<-"  */
    T_mod = 271,                   /* "mod"  */
    T_and = 272,                   /* "and"  */
    T_or = 273,                    /* "or"  */
    T_not = 274,                   /* "not"  */
    T_div = 275,                   /* "div"  */
    T_lte = 276,                   /* "<="  */
    T_gte = 277,                   /* ">="  */
    T_id = 278,                    /* T_id  */
    T_const = 279,                 /* T_const  */
    T_character = 280,             /* T_character  */
    T_string = 281                 /* T_string  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 152 "parser.y"

  int num;
  char *character;

  char *op;
  char *var;
  char *str;

  Function *fun;
  Func_Decl *func_decl;
  Func_Call *func_call;
  Block *blk;
  Stmt *stmt;
  Expr *expr;
  Cond *cond;

  Full_Type *type;

  std::vector<Decl *> *def_list;
  std::vector<Stmt *> *stmt_list;
  std::vector<Expr *> *expr_list;
  std::vector<std::string> *id_list;

#line 114 "parser.hpp"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;


int yyparse (void);


#endif /* !YY_YY_PARSER_HPP_INCLUDED  */
