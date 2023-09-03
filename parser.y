%{
#include <cstdio>
#include <cstdlib>
#include <string>
#include <cstring>
#include <vector>
#include <fstream>
#include <unistd.h>
#include <fcntl.h>

#include "cxxopts.hpp"
#include "ast.hpp"
#include "lexer.hpp"

SymbolTable st;

std::vector<std::string> names;
std::vector<Function *> runtime_funcs;

void fill_names() {
    names.push_back("and");
    names.push_back("char");
    names.push_back("div");
    names.push_back("do");
    names.push_back("else");
    names.push_back("fun");
    names.push_back("if");
    names.push_back("int");
    names.push_back("mod");
    names.push_back("not");
    names.push_back("nothing");
    names.push_back("or");
    names.push_back("ref");
    names.push_back("return");
    names.push_back("then");
    names.push_back("var");
    names.push_back("while");
}

void init_runtime() {
    std::vector<Decl *> *v1 = new std::vector<Decl *>(); v1->push_back(new Var_Decl("n", INT_t, false, 0));
    Func_Decl *header1 = new Func_Decl("writeInteger", v1, NO_t);
    runtime_funcs.push_back(new Function(header1));

    std::vector<Decl *> *v2 = new std::vector<Decl *>(); v2->push_back(new Var_Decl("c", CHAR_t, false, 0));
    Func_Decl *header2 = new Func_Decl("writeChar", v2, NO_t);
    runtime_funcs.push_back(new Function(header2));

    std::vector<Decl *> *v3 = new std::vector<Decl *>(); v3->push_back(new Var_Decl("s", STRING_t, true, 1));
    Func_Decl *header3 = new Func_Decl("writeString", v3, NO_t);
    runtime_funcs.push_back(new Function(header3));

    std::vector<Decl *> *v4 = new std::vector<Decl *>();
    Func_Decl *header4 = new Func_Decl("readInteger", v4, INT_t);
    runtime_funcs.push_back(new Function(header4));

    std::vector<Decl *> *v5 = new std::vector<Decl *>();
    Func_Decl *header5 = new Func_Decl("readChar", v5, CHAR_t);
    runtime_funcs.push_back(new Function(header5));

    std::vector<Decl *> *v6 = new std::vector<Decl *>(); v6->push_back(new Var_Decl("n", INT_t, false, 0)); v6->push_back(new Var_Decl("s", STRING_t, true, 1));
    Func_Decl *header6 = new Func_Decl("readString", v6, NO_t);
    runtime_funcs.push_back(new Function(header6));

    std::vector<Decl *> *v7 = new std::vector<Decl *>(); v7->push_back(new Var_Decl("c", CHAR_t, false, 0));
    Func_Decl *header7 = new Func_Decl("ascii", v7, INT_t);
    runtime_funcs.push_back(new Function(header7));

    std::vector<Decl *> *v8 = new std::vector<Decl *>(); v8->push_back(new Var_Decl("n", INT_t, false, 0));
    Func_Decl *header8 = new Func_Decl("chr", v8, CHAR_t);
    runtime_funcs.push_back(new Function(header8));

    std::vector<Decl *> *v9 = new std::vector<Decl *>(); v9->push_back(new Var_Decl("s", STRING_t, true, 1));
    Func_Decl *header9 = new Func_Decl("strlen", v9, INT_t);
    runtime_funcs.push_back(new Function(header9));

    std::vector<Decl *> *v10 = new std::vector<Decl *>(); v10->push_back(new Var_Decl("s1", STRING_t, true, 1)); v10->push_back(new Var_Decl("s2", STRING_t, true, 1));
    Func_Decl *header10 = new Func_Decl("strcmp", v10, INT_t);
    runtime_funcs.push_back(new Function(header10));

    std::vector<Decl *> *v11 = new std::vector<Decl *>(); v11->push_back(new Var_Decl("trg", STRING_t, true, 1)); v11->push_back(new Var_Decl("src", STRING_t, true, 1));
    Func_Decl *header11 = new Func_Decl("strcpy", v11, NO_t);
    runtime_funcs.push_back(new Function(header11));

    std::vector<Decl *> *v12 = new std::vector<Decl *>(); v12->push_back(new Var_Decl("trg", STRING_t, true, 1)); v12->push_back(new Var_Decl("src", STRING_t, true, 1));
    Func_Decl *header12 = new Func_Decl("strcat", v12, NO_t);
    runtime_funcs.push_back(new Function(header12));
}

void delete_runtime() {
    for(auto &x: runtime_funcs) delete x;
}

std::vector<Decl *> *concat(std::vector<Decl *> *v1, std::vector<Decl *> *v2) {
    auto v = v1;
    for(auto &x: *v2) {
        v->push_back(x);
    }
    delete v2;
    return v;
}

std::vector<int> *concat(std::vector<int> *v1, std::vector<int> *v2) {
  auto v = v2;
  if(v1 == nullptr) return v;
  for(auto &x: *v1) {
    v->push_back(x);
  }
  delete v1;
  return v;
}

std::vector<int> *concat_rev(std::vector<int> *v1, std::vector<int> *v2) {
  auto v = v1;
  if(v2 == nullptr) return v;
  for(auto &x: *v2) {
    // std::cout << v2->size() << std::endl;
    v->push_back(x);
  }
  delete v2;
  return v;
}

bool main_f = false;
bool optimize;

%}

%token T_char        "char"
%token T_do          "do"
%token T_else        "else"
%token T_fun         "fun"
%token T_if          "if"
%token T_int         "int"
%token T_nothing     "nothing"
%token T_ref         "ref"
%token T_return      "return"
%token T_then        "then"
%token T_var         "var"
%token T_while       "while"
%token T_assign      "<-"

%token T_mod         "mod"
%token T_and         "and"
%token T_or          "or"
%token T_not         "not"
%token T_div         "div"
%token T_lte         "<="
%token T_gte         ">="

%union {
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
}

%token<var> T_id
%token<num> T_const
%token<character> T_character
%token<str> T_string

%left<op> T_or
%left<op> T_and
%nonassoc<op> T_not
%nonassoc<op> '=' '#' '>' '<' T_lte T_gte
%left<op> '+' '-'
%left<op> '*' T_div T_mod

%type<fun> func_def func_decl
%type<func_decl> header
%type<def_list> fpar_def_list fpar_def local_def_list local_def var_def fpar_def_opt
%type<id_list> id_list
%type<type> data_type fpar_type ret_type type brackets
%type<blk> block
%type<stmt_list> stmt_list
%type<stmt> stmt
%type<expr> l_value expr
%type<cond> cond
%type<func_call> func_call
%type<expr_list> expr_opt expr_list

%expect 1

%%

program : func_def    {
    st.push_scope();
        init_runtime();
        for(auto &r: runtime_funcs) {
            r->analyze();
        }
        st.push_scope();
            $1->analyze();
            $1->llvm_compile_and_dump(optimize);
            delete $1;
        st.pop_scope();
        delete_runtime();
    st.pop_scope();

}
;

func_def : header local_def_list block    { $$ = new Function($1, $2, $3); }
;

header : "fun" T_id '(' fpar_def_opt ')' ':' ret_type   { if(main_f) $$ = new Func_Decl($2, $4, $7->type); else { main_f = true; $$ = new Main_Func($2, $4, $7->type); } }
;

fpar_def_opt : /* nothing */    { $$ = new std::vector<Decl *>(); }
|   fpar_def_list               { $$ = $1; }
;

fpar_def_list :
    fpar_def                         { $$ = $1; }
|   fpar_def_list ';' fpar_def       { $$ = concat($1, $3); }
;


fpar_def :
    id_list ':' fpar_type         { if($3->dim) yyerror("Missing 'ref'"); $$ = new std::vector<Decl *>(); for(auto &x: *$1) $$->push_back(new Var_Decl(x, $3->type, false, 0, nullptr)); }
|   "ref" id_list ':' fpar_type   { $$ = new std::vector<Decl *>(); for(auto &x: *$2) $$->push_back(new Var_Decl(x, $4->type, true, $4->dim, new std::vector<int>(*$4->dim_sizes))); delete $4; }
;

id_list :
    T_id                { $$ = new std::vector<std::string>(); $$->push_back($1); }
|   T_id ',' id_list    { $3->emplace($3->begin(), $1); $$ = $3; }
;

fpar_type :
    data_type                           { $$ = $1; }
|   data_type '[' ']' brackets          { if($1->type == INT_t) $1->type = ARRAY_t; else $1->type = STRING_t; $$ = $4; $$->type = $1->type; $$->dim++; $$->dim_sizes = concat_rev(new std::vector<int>(1, 4096), $4->dim_sizes); delete $1; }
|   data_type '[' T_const ']' brackets  { if($1->type == INT_t) $1->type = ARRAY_t; else $1->type = STRING_t; $$ = $5; $$->type = $1->type; $$->dim++; $$->dim_sizes = concat_rev(new std::vector<int>(1, $3), $5->dim_sizes); delete $1; }
;

type :
    data_type                           { $$ = $1; }
|   data_type '[' T_const ']' brackets  { if($1->type == INT_t) $1->type = ARRAY_t; else $1->type = STRING_t; $$ = $5; $$->type = $1->type; $$->dim++; $$->dim_sizes = concat_rev(new std::vector<int>(1, $3), $5->dim_sizes); delete $1; }
;

data_type :
    T_int   { $$ = new Full_Type(INT_t); }
|   T_char  { $$ = new Full_Type(CHAR_t); }
;

brackets : /* nothing */        { $$ = new Full_Type(0, 0); }
|   brackets '[' T_const ']'    { $$ = new Full_Type($3, 1); $$->copy($1); }
;

ret_type :
    data_type    { $$ = $1; }
|   "nothing"    { $$ = new Full_Type(NO_t); }
;

local_def_list : /* nothing */      { $$ = new std::vector<Decl *>(); }
|   local_def_list local_def        { $$ = concat($1, $2); }
;

local_def :
    func_def    { $$ = new std::vector<Decl *>(); $$->push_back($1); }
|   func_decl   { $$ = new std::vector<Decl *>(); $$->push_back($1); }
|   var_def     { $$ = $1; }
;

func_decl : header ';'  { $$ = new Function($1); }
;

var_def : "var" id_list ':' type ';'    {
    $$ = new std::vector<Decl *>();

    for(auto &id: *$2) $$->push_back(new Var_Decl(id, $4->type, false, $4->dim, $4->dim_sizes /* new std::vector<int>(*$4->dim_sizes) */));
    //delete $4;
}
;

block : '{' stmt_list '}'   { $$ = new Block($2); }
;

stmt_list : /* nothing */   { $$ = new std::vector<Stmt *>(); }
|   stmt_list stmt          { $1->push_back($2); $$ = $1; }
;

stmt :
    ';'                                     { $$ = nullptr; }
|   l_value "<-" expr ';'                   { $$ = new Assign($1, $3); }
|   block                                   { $$ = $1; }
|   func_call ';'                           { $$ = $1; }
|   "if" cond "then" stmt                   { $$ = new If($2, $4); }
|   "if" cond "then" stmt "else" stmt       { $$ = new If($2, $4, $6); }
|   "while" cond "do" stmt                  { $$ = new While($2, $4); }
|   "return" ';'                            { $$ = new Return(NO_t); }
|   "return" expr ';'                       { $$ = new Return($2); }
;

l_value :
    T_id                    { $$ = new Id($1); }
|   l_value '[' expr ']'    { $$ = $1; $$->set_array(); $$->add_expr($3); }
;

expr :
    T_const                 { $$ = new IntConst($1); }
|   T_character             { $$ = new CharConst($1); }
|   T_string                { $$ = new StringConst($1); }
|   l_value                 { $$ = $1; }
|   '(' expr ')'            { $$ = $2; }
|   func_call               { $$ = $1; }
|   '+' expr                { $$ = new SignedInt($1, $2); }
|   '-' expr                { $$ = new SignedInt($1, $2); }
|   expr '+' expr           { $$ = new BinOp($1, $2, $3); }
|   expr '-' expr           { $$ = new BinOp($1, $2, $3); }
|   expr '*' expr           { $$ = new BinOp($1, $2, $3); }
|   expr "div" expr         { $$ = new BinOp($1, $2, $3); }
|   expr "mod" expr         { $$ = new BinOp($1, $2, $3); }
;

func_call : T_id '(' expr_opt ')'   { $$ = new Func_Call($1, $3); }
;

expr_opt : /* nothing */    { $$ = new std::vector<Expr *>(); }
|   expr_list               { $$ = $1; }
;

expr_list :
    expr                    { $$ = new std::vector<Expr *>(); $$->push_back($1); }
|   expr ',' expr_list      { $3->emplace($3->begin(), $1); $$ = $3; }
;

cond :
    '(' cond ')'            { $$ = $2; }
|   "not" cond              { $$ = new NotCond($2); }
|   cond "and" cond         { $$ = new BinCond($1, $2, $3); }
|   cond "or" cond          { $$ = new BinCond($1, $2, $3); }
|   expr '=' expr           { $$ = new BinOpCond($1, $2, $3); }
|   expr '#' expr           { $$ = new BinOpCond($1, $2, $3); }
|   expr '<' expr           { $$ = new BinOpCond($1, $2, $3); }
|   expr '>' expr           { $$ = new BinOpCond($1, $2, $3); }
|   expr "<=" expr          { $$ = new BinOpCond($1, $2, $3); }
|   expr ">=" expr          { $$ = new BinOpCond($1, $2, $3); }
;

%%

void yyerror(const char* msg) {
    if(!strcmp(msg, "syntax error")) {
        fprintf(stderr, "Error: %s in line %d.\n", msg, lineno);
    } else {
        fprintf(stderr, "Error: %s.\n", msg);
    }
    exit(1);
}

int main(int argc, char** argv) {
    fill_names();

    cxxopts::Options options("Grace Compiler", "A compiler project for NTUA's Grace programming language.");

    options.add_options()
        ("o,optimize", "Optimize code")
        ("f,final", "Generate final code to standard output from standard input")
        ("i,intermediate", "Generate intermediade representation to standard output from standard input")
        ("h,help", "Print usage info")
        ("filename", "The grace code to compile", cxxopts::value<std::string>())
    ;

    options.parse_positional({"filename"});
    cxxopts::ParseResult result;
    try {
      result = options.parse(argc, argv);
    } catch (const std::exception& ex) {
      std::cerr << ex.what() << ". Emit '-h' flag for usage." << std::endl;
      exit(1);
    }

    if (!result.arguments().size()) {
        std::cout << options.help() << std::endl;
        exit(0);
    }

    if (result.count("help")) {
        std::cout << options.help() << std::endl;
        exit(0);
    }

    optimize = result["optimize"].as<bool>();
    
    if(result["final"].as<bool>()) {
        yyparse();
        AST::compile_to_asm();
        exit(0);
    }

    if(result["intermediate"].as<bool>()) {
        yyparse();
        AST::TheModule->print(llvm::outs(), nullptr);
        exit(0);
    }

    std::string filename;
    if(result.count("filename")) {
        filename = result["filename"].as<std::string>();

        auto found = filename.find_last_of('.');
        auto title = filename.substr(0, found);

        std::ifstream file(filename, std::ios::in);
        if(!file.is_open()) {
            std::cerr << "gracec: could not open file " << filename << std::endl;
            exit(1);
        }

        int fd = open(filename.c_str(), O_RDONLY);
        if(fd == -1) {
            std::cerr << "gracec: could not open file " << filename << std::endl;
            exit(1);
        }

        if(dup2(fd, 0) == -1) {
            std::cerr << "gracec: dup2 failed" << std::endl;
            exit(1);
        }
        close(fd);

        yyparse();
        file.close();

        std::error_code error;
        llvm::raw_fd_ostream _imm(title + ".imm", error);
        if(!error) {
            AST::TheModule->print(_imm, nullptr);
            _imm.flush();
        } else {
            std::cerr << "gracec: error creating file" << std::endl;
            exit(1);
        }

        char command[256];
        sprintf(command, "llc -o %s.asm %s.imm", title.c_str(), title.c_str());
        if(system(command)) {
            std::cerr << "gracec: error compiling imm code" << std::endl;
            exit(1);
        }
    }
    
    return 0;
}
