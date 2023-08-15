#include "ast.hpp"

llvm::LLVMContext AST::TheContext;
llvm::IRBuilder<> AST::Builder(TheContext);
std::unique_ptr<llvm::Module> AST::TheModule;
std::unique_ptr<llvm::legacy::FunctionPassManager> AST::TheFPM;

std::map<std::string, llvm::Value*> AST::NamedValues;
std::map<llvm::Value *, llvm::Type *> AST::PointerTypes;
std::map<std::string, std::pair<llvm::Function*, std::vector<char>>> AST::NamedFunctions;
std::map<std::string, std::pair<llvm::Value *, std::pair<llvm::Type *, llvm::Type *>>> AST::FunctionArguments;
std::vector<llvm::Function *> AST::FunctionStack;
std::vector<llvm::Function *> AST::LocalFunctions;

unsigned AST::naming_idx = 0;
llvm::Function *AST::TheWriteInteger;
llvm::Function *AST::TheWriteString;
llvm::Function *AST::TheWriteChar;
llvm::Function *AST::TheReadInteger;
llvm::Function *AST::TheReadChar;
llvm::Function *AST::TheReadString;
llvm::Function *AST::TheAscii;
llvm::Function *AST::TheChr;
llvm::Function *AST::TheStrlen;
llvm::Function *AST::TheStrcmp;
llvm::Function *AST::TheStrcpy;
llvm::Function *AST::TheStrcat;

llvm::Type *AST::i8;
llvm::Type *AST::i32;
llvm::Type *AST::i64;
