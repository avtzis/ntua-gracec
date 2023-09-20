#ifndef __AST_HPP__
#define __AST_HPP__

#include <cstdio>
#include <cstring>
#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <algorithm>

#include "symbol.hpp"

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IR/Attributes.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>
#include <llvm/Transforms/Scalar/GVN.h>
#include <llvm/Transforms/Utils.h>
#include <llvm/MC/TargetRegistry.h>
#include <llvm/Support/FileSystem.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/Host.h>
#include <llvm/Target/TargetMachine.h>
#include <llvm/Target/TargetOptions.h>

extern std::vector<std::string> names;
extern std::vector<int> *concat(std::vector<int> *v1, std::vector<int> *v2);
extern std::vector<int> *concat_rev(std::vector<int> *v1, std::vector<int> *v2);

extern std::vector<std::string> rt_funcs;

struct Full_Type {
    Type type;
    int dim;
    std::vector<int> *dim_sizes;
    Full_Type(Type t, std::vector<int> *ds=nullptr, int d=0): type(t), dim_sizes(ds), dim(d) {
      if(dim_sizes == nullptr) dim_sizes = new std::vector<int>();
    }
    Full_Type(int ds, int d=0): dim(d) {
      if(dim_sizes == nullptr) dim_sizes = new std::vector<int>();
      if(ds != 0) dim_sizes->push_back(ds);
    }
    void copy(Full_Type* FT) {
      if(FT->dim_sizes != nullptr) {
        if(dim_sizes == nullptr) dim_sizes = new std::vector<int>();
        dim_sizes = concat(dim_sizes, FT->dim_sizes);
        dim += FT->dim;
      }
      // delete FT;
    }
    Full_Type() {}
    ~Full_Type() {
      delete dim_sizes;
    }
};

class AST {
public:
  virtual ~AST() = default;
  virtual void analyze() = 0;
  virtual llvm::Value* codegen(bool AInst=false) const { return nullptr; }
  void llvm_compile_and_dump(bool optimize=true) {
    // Initialize
    TheModule = std::make_unique<llvm::Module>("grace program", TheContext);
    TheFPM = std::make_unique<llvm::legacy::FunctionPassManager>(TheModule.get());
    if(optimize) {
      TheFPM->add(llvm::createPromoteMemoryToRegisterPass());
      TheFPM->add(llvm::createInstructionCombiningPass());
      TheFPM->add(llvm::createReassociatePass());
      TheFPM->add(llvm::createGVNPass());
      TheFPM->add(llvm::createCFGSimplificationPass());
    }
    TheFPM->doInitialization();

    // Initialize types
    i8 = llvm::IntegerType::get(TheContext, 8);
    i32 = llvm::IntegerType::get(TheContext, 32);
    i64 = llvm::IntegerType::get(TheContext, 64);

    // Initialize library functions
    llvm::FunctionType *writeInteger_type =llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i64}, false);
    TheWriteInteger = llvm::Function::Create(writeInteger_type, llvm::Function::ExternalLinkage, "writeInteger", TheModule.get());

    llvm::FunctionType *writeString_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {llvm::PointerType::get(i8, 0)}, false);
    TheWriteString = llvm::Function::Create(writeString_type, llvm::Function::ExternalLinkage, "writeString", TheModule.get());

    llvm::FunctionType *writeChar_type =llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i8}, false);
    TheWriteChar = llvm::Function::Create(writeChar_type, llvm::Function::ExternalLinkage, "writeChar", TheModule.get());

    llvm::FunctionType *readInteger_type =llvm::FunctionType::get(llvm::Type::getInt32Ty(TheContext), {}, false);
    TheReadInteger = llvm::Function::Create(readInteger_type, llvm::Function::ExternalLinkage, "readInteger", TheModule.get());

    llvm::FunctionType *readChar_type =llvm::FunctionType::get(llvm::Type::getInt8Ty(TheContext), {}, false);
    TheReadChar = llvm::Function::Create(readChar_type, llvm::Function::ExternalLinkage, "readChar", TheModule.get());

    llvm::FunctionType *readString_type =llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {i64, llvm::PointerType::get(i8, 0)}, false);
    TheReadString = llvm::Function::Create(readString_type, llvm::Function::ExternalLinkage, "readString", TheModule.get());

    llvm::FunctionType *ascii_type =llvm::FunctionType::get(llvm::Type::getInt32Ty(TheContext), {i8}, false);
    TheAscii = llvm::Function::Create(ascii_type, llvm::Function::ExternalLinkage, "ascii", TheModule.get());

    llvm::FunctionType *chr_type =llvm::FunctionType::get(llvm::Type::getInt8Ty(TheContext), {i64}, false);
    TheChr = llvm::Function::Create(chr_type, llvm::Function::ExternalLinkage, "chr", TheModule.get());

    llvm::FunctionType *strlen_type = llvm::FunctionType::get(llvm::Type::getInt32Ty(TheContext), {llvm::PointerType::get(i8, 0)}, false);
    TheStrlen = llvm::Function::Create(strlen_type, llvm::Function::ExternalLinkage, "strlen", TheModule.get());

    llvm::FunctionType *strcmp_type = llvm::FunctionType::get(llvm::Type::getInt32Ty(TheContext), {llvm::PointerType::get(i8, 0), llvm::PointerType::get(i8, 0)}, false);
    TheStrcmp = llvm::Function::Create(strcmp_type, llvm::Function::ExternalLinkage, "strcmp", TheModule.get());

    llvm::FunctionType *strcpy_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {llvm::PointerType::get(i8, 0), llvm::PointerType::get(i8, 0)}, false);
    TheStrcpy = llvm::Function::Create(strcpy_type, llvm::Function::ExternalLinkage, "strcpy", TheModule.get());

    llvm::FunctionType *strcat_type = llvm::FunctionType::get(llvm::Type::getVoidTy(TheContext), {llvm::PointerType::get(i8, 0), llvm::PointerType::get(i8, 0)}, false);
    TheStrcat = llvm::Function::Create(strcat_type, llvm::Function::ExternalLinkage, "strcat", TheModule.get());

    // Emit the program code.
    auto main = (llvm::Function*)codegen();

    // Verify the IR.
    // bool bad = llvm::verifyModule(*TheModule, &llvm::errs());
    if (false) {
      std::cerr << "The IR is bad!" << std::endl;
      TheModule->print(llvm::errs(), nullptr);
      std::exit(1);
    }

    // Optimize!
    TheFPM->run(*main);

    // Print out the IR.
    // TheModule->print(llvm::outs(), nullptr);
  }

  static std::unique_ptr<llvm::Module> TheModule;

  static void compile_to_asm() {
    auto CPU = "generic";
    auto Features = "";

    // auto TargetTriple("x86_64-pc-linux-gnu");
    auto TargetTriple = llvm::sys::getDefaultTargetTriple();
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    std::string Error;
    auto Target = llvm::TargetRegistry::lookupTarget(TargetTriple, Error);
    if(!Target) {
      llvm::errs() << Error << "\n";
      exit(1);
    }

    llvm::TargetOptions opt;
    auto RM = llvm::Optional<llvm::Reloc::Model>();
    auto TargetMachine = Target->createTargetMachine(TargetTriple, CPU, Features, opt, RM);

    TheModule->setDataLayout(TargetMachine->createDataLayout());
    TheModule->setTargetTriple(TargetTriple);

    llvm::legacy::PassManager pass;
    auto FileType = llvm::CGFT_AssemblyFile;

    if(TargetMachine->addPassesToEmitFile(pass, llvm::outs(), nullptr, FileType)) {
      llvm::errs() << "TargetMachine can't emit a file of this type\n";
      exit(1);
    }

    pass.run(*TheModule);
  }

protected:
  static llvm::LLVMContext TheContext;
  static llvm::IRBuilder<> Builder;
  static std::unique_ptr<llvm::legacy::FunctionPassManager> TheFPM;

  static std::map<std::string, llvm::Value*> NamedValues;
  static std::map<llvm::Value *, llvm::Type *> PointerTypes;
  static std::map<std::string, std::pair<llvm::Function *, std::vector<char>>> NamedFunctions;
  static std::map<std::string, std::pair<llvm::Value *, std::pair<llvm::Type *, llvm::Type *>>> FunctionArguments;
  static std::vector<std::string> FunctionArgumentStack;
  static std::vector<llvm::Function *> FunctionStack;
  static std::vector<llvm::Function *> LocalFunctions;

  static unsigned naming_idx;

  static llvm::Function *TheWriteInteger;
  static llvm::Function *TheWriteString;
  static llvm::Function *TheWriteChar;
  static llvm::Function *TheReadInteger;
  static llvm::Function *TheReadChar;
  static llvm::Function *TheReadString;
  static llvm::Function *TheAscii;
  static llvm::Function *TheChr;
  static llvm::Function *TheStrlen;
  static llvm::Function *TheStrcmp;
  static llvm::Function *TheStrcpy;
  static llvm::Function *TheStrcat;

  static llvm::Type *i8;
  static llvm::Type *i32;
  static llvm::Type *i64;

  static llvm::ConstantInt* c8(char c) {
    return llvm::ConstantInt::get(TheContext, llvm::APInt(8, c, true));
  }
  static llvm::ConstantInt* c32(int n) {
    return llvm::ConstantInt::get(TheContext, llvm::APInt(32, n, true));
  }
  static llvm::ConstantInt* c64(long n) {
    return llvm::ConstantInt::get(TheContext, llvm::APInt(64, n, true));
  }

  static llvm::Type* parse_type(Type t, int size=64, bool ref=false) {
    switch(t) {
      case 0: return ref ? llvm::PointerType::get(i32, 0) : i32;
      case 1: return ref ? llvm::PointerType::get(i8, 0) : i8;
      case 2: if(ref) return llvm::PointerType::get(i32, 0); else return llvm::ArrayType::get(i32, size);  /////!array
      case 3: if(ref) return llvm::PointerType::get(i8, 0); else return llvm::ArrayType::get(i8, size+1);  /////!array
      case 5: return llvm::Type::getVoidTy(TheContext);
      default: return nullptr;
    }
  }

  static llvm::AllocaInst *CreateEntryBlockAlloca(llvm::Function *TheFunction, const llvm::StringRef &VarName, llvm::Type *VarType) {
    llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
    return TmpB.CreateAlloca(VarType, nullptr, VarName);
  }

  static auto *GetVal(llvm::Value * loadedValue) {
    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
    llvm::AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, "ldtmp", loadedValue->getType());
    Builder.CreateStore(loadedValue, Alloca);
    return Alloca;
  }
};

class Decl: public AST {
public:
  virtual ~Decl() = default;
  virtual void analyze() = 0;
  virtual void insert(bool defined) {};
  virtual void param_insert(std::string func_name) {};
  virtual void declare(bool declaring=true) {}
  virtual bool declared(bool current) { return 0; }
  virtual std::string getId() { return ""; }
  virtual Type getType() { return NO_t; }
  virtual bool getRef() { return false; }
  virtual int getDims() { return 0; }
  virtual int linear_size() const { return 0; }
};

class Stmt: public AST {
public:
  virtual ~Stmt() = default;
  virtual void analyze() = 0;
};

class Cond: public AST {
public:
  virtual void analyze() = 0;
};

class Block: public Stmt {
public:
  Block(std::vector<Stmt *> *s): stmt_list(s) {}
  ~Block() {
    for(auto &s: *stmt_list) delete s;
    delete stmt_list;
  }
  void analyze() override {
    for (Stmt *s: *stmt_list) {
      if(s == nullptr) continue;
      s->analyze();
    }
  }
  llvm::Value* codegen(bool AInst=false) const override {
    for(Stmt *s: *stmt_list) {
      if(s == nullptr) continue;
      s->codegen();
    }
    return nullptr;
  }

private:
  std::vector<Stmt *> *stmt_list;
};

class Expr: public AST {
public:
  void check_type(Type t, int dim, std::vector<int> s=std::vector<int>()) {
    analyze();
    if (type != t || dimensions != dim) {
      std::cerr << getId() << " -> got: type " << type << " dimensions " << dimensions << std::endl;
      std::cerr << getId() << " -> expected: type " << t << " dimensions " << dim << std::endl;
      yyerror("Type mismatch"); 
    }

    if (dim >= 2 && !s.empty()) {
      auto sizes = getSizes();
      for(unsigned i = 1; i < dim; ++i) {
        if(sizes[i] != s[i]) {
          char error_msg[128];
          char *cstr_id = new char[getId().length() + 1];
          strcpy(cstr_id, getId().c_str());
          sprintf(error_msg, "%s -> argument of type '%s (*)[%d]' is incompatible with parameter of type '%s (*)[%d]'", cstr_id, parse_arr_type(type), sizes[i], parse_arr_type(t), s[i]);
          delete[] cstr_id;
          yyerror(error_msg);
        }
      }
    }
  }
  const char* parse_arr_type(Type type) const  {
    return type == 2 ? "int" : "char";
  }
  void check_type_cond() {
    analyze();
    if (type != INT_t && type != CHAR_t) yyerror("Type mismatch in condition");
  }
  Type getType() { return type; }
  int getDim() { return dimensions; }
  virtual void set_array() {}
  virtual void add_expr(Expr *e) {}
  virtual bool is_lval() { return false; }
  virtual std::string getId() const { return ""; }
  virtual std::vector<int> getSizes() const { return std::vector<int>(); }

protected:
  Type type;
  int dimensions;
};

class Func_Decl: public Decl {
public:
  Func_Decl(std::string i, std::vector<Decl *> *p, Type r): id(i), param_list(p), ret(r) {}
  ~Func_Decl() {
    for(Decl *d: *param_list) delete d;
    delete param_list;
  }
  void insert(bool defined) override {
    st.insert(id, FUNC_t, ret, 0, false, nullptr, defined);
  }
  virtual void analyze() override {
    //* lookup previously declared function and check if params are the same
    auto *entry = st.lookup_last_scope(id);    //! lookup should be in the last scope
    if(!entry->defined) {
      declaration = true;
      auto st_params = st.get_params(id);
      auto st_params_length = st_params.size();
      auto params_length = param_list ? param_list->size() : 0;

      if(params_length != st_params_length) {
        char error_msg[128];
        char *cstr = new char[id.length() + 1];
        strcpy(cstr, id.c_str());
        sprintf(error_msg, "In function definition '%s()', expected %lu paramater(s) but got %lu", cstr, st_params_length, params_length);
        delete[] cstr;
        yyerror(error_msg);
      }

      //* check for every param if id-type-ref-dims are same
      auto it_p = param_list->begin();    //? params are backwards?
      auto it_s = st_params.begin();
      for(int p_count = 1; it_s != st_params.end(); ++it_s, ++it_p, ++p_count) {
        if(it_s->id != (*it_p)->getId() || it_s->type != (*it_p)->getType()
        || it_s->ref != (*it_p)->getRef() || it_s->dimenesions != (*it_p)->getDims()) {
          char error_msg[128];
          char *cstr = new char[id.length() + 1];
          strcpy(cstr, id.c_str());
          sprintf(error_msg, "In function definition '%s()', variable %d cannot be matched with previously declared function", cstr, p_count);
          delete[] cstr;
          yyerror(error_msg);
        }
      }

      //* check if function ret type is same
      if(entry->ret != ret) {
        char error_msg[128];
        char *cstr = new char[id.length() + 1];
        strcpy(cstr, id.c_str());
        sprintf(error_msg, "In function definition '%s()', return type cannot be matched with previously declared function", cstr);
        delete[] cstr;
        yyerror(error_msg);
      }

      entry->defined = true;
    }
  }
  void declare(bool declaring=true) override {
    for(Decl *d: *param_list) {
      d->analyze();
      if(declaring) d->param_insert(id);
    }
  }
  bool declared(bool current) override {
    auto *entry = current ? st.lookup_current_scope(id) : st.lookup_last_scope(id);
    if(!entry) return false;
    return !entry->defined;
  }
  virtual llvm::Function* codegen(bool AInst=false) const override {
    // if(AInst) return nullptr;

    for(auto &x: *param_list) {
      auto it = FunctionArguments.find(x->getId());
      if(it != FunctionArguments.end()) {  // found
        auto it_fas = std::find(FunctionArgumentStack.begin(), FunctionArgumentStack.end(), it->first);
        
        auto nodeHandler = FunctionArguments.extract(it->first);
        auto newName = it->first + "." + std::to_string(++naming_idx);
        nodeHandler.key() = newName;
        FunctionArguments.insert(std::move(nodeHandler));

        *it_fas = newName;
      }
      //! argument should stay if previous function called in current context
    }

    std::vector<llvm::Type*> ArgsTypes;

    //TODO: push arguments from FunctionArguments before function's arguments
    //! these arguments MUST be ptr (by ref), in order to be mutable
    /* for(auto &x: FunctionArguments) {
      ArgsTypes.push_back(x.second.second.second);
    } */
    for(auto &x: FunctionArgumentStack) {
      ArgsTypes.push_back(FunctionArguments[x].second.second);
    }

    for(Decl *d: *param_list) {
      ArgsTypes.push_back(parse_type(d->getType(), d->linear_size(), d->getRef()));
    }

    llvm::FunctionType *FT = llvm::FunctionType::get(parse_type(ret), ArgsTypes, false);

    llvm::Function *F = llvm::Function::Create(FT, llvm::Function::ExternalLinkage, id, TheModule.get());

    unsigned i = 0;
    signed n = FunctionArgumentStack.size();
    auto it = FunctionArgumentStack.begin();
    for (auto &Arg : F->args()) {
      if(n-- > 0) {
        Arg.setName(*(it++));
        //! inherited argument should not have its native name if new arguments have the same name
      } else {
        Arg.setName((*param_list)[i++]->getId());
      }
    }

    NamedFunctions[id] = {F, getRefs()};

    return F;
  }
  virtual bool isMain() { return false; }
  std::string getId() override {
    return id;
  }
  std::vector<char> getRefs() const {
    std::vector<char> result;
    for(auto &x: *param_list) {
      result.push_back(x->getRef() ? 1 : 0);
    }
    return result;
  }
  auto getTypes() {
    std::vector<llvm::Type *> result;
    for(Decl *d: *param_list) {
      result.push_back(parse_type(d->getType(), d->linear_size(), false));
    }
    return result;
  }
  auto getTypesRef() {
    std::vector<llvm::Type *> result;
    for(Decl *d: *param_list) {
      result.push_back(parse_type(d->getType(), d->linear_size(), true));
    }
    return result;
  }

  bool isDecl() {
    return declaration;
  }

protected:
  std::string id;
  std::vector<Decl *> *param_list;
  Type ret;
  bool declaration = false;
};

class Function: public Decl {
public:
  Function(Func_Decl *h, std::vector<Decl *> *d=nullptr, Block *b=nullptr): header(h), local_def_list(d), block(b) {}
  ~Function() {
    delete header;
    if(local_def_list != nullptr) for(Decl *d: *local_def_list) delete d;
    delete local_def_list;
    delete block;
  }
  void analyze() override {
    bool declaring = local_def_list == nullptr;

    if(header->declared(true)) {
      if(declaring) yyerror("Function already declared in scope");
    } else {
      header->insert(!declaring);
    }

    st.push_scope();

    if(declaring) {
      header->declare();
    } else {
      if(!header->declared(false)) header->declare();
      else header->declare(false);
      header->analyze();
      if(local_def_list != nullptr) for(auto &d: *local_def_list) d->analyze();
      if(block != nullptr) block->analyze();
    }

    st.pop_scope();
  }
  llvm::Function* codegen(bool AInst=false) const override {          //// extern??
    //TODO: save FunctionArguments state
    auto oldArguments(FunctionArguments);
    auto oldArgumentStack(FunctionArgumentStack);

    if(!local_def_list) {
      auto ret = header->codegen(true);
      FunctionArguments = oldArguments;
      FunctionArgumentStack = oldArgumentStack;
      return ret;
    }

    llvm::Function *TheFunction = header->isDecl() ? NamedFunctions[header->getId()].first : header->codegen();

    llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "entry", TheFunction);
    Builder.SetInsertPoint(BB);

    auto OldBindings(NamedValues);

    if(header->isMain()) {
      NamedFunctions["writeInteger"] = {nullptr, {0}};
      NamedFunctions["writeString"] = {nullptr, {1}};
      NamedFunctions["writeChar"] = {nullptr, {0}};
      NamedFunctions["readInteger"] = {nullptr, {}};
      NamedFunctions["readChar"] = {nullptr, {}};
      NamedFunctions["readString"] = {nullptr, {0, 1}};
      NamedFunctions["ascii"] = {nullptr, {0}};
      NamedFunctions["chr"] = {nullptr, {0}};
      NamedFunctions["strlen"] = {nullptr, {1}};
      NamedFunctions["strcmp"] = {nullptr, {1, 1}};
      NamedFunctions["strcpy"] = {nullptr, {1, 1}};
      NamedFunctions["strcat"] = {nullptr, {1, 1}};
    } else {
      FunctionStack.push_back(TheFunction);
    }

    // NamedFunctions[header->getId()] = {TheFunction, header->getRefs()};
    auto OldFunctions(NamedFunctions);

    unsigned i = 0;
    auto types = header->getTypes();
    auto refs = header->getRefs();
    signed n = TheFunction->arg_size() - refs.size();
    auto typesRef = header->getTypesRef();
    for (auto &Arg : TheFunction->args()) {
      //TODO: sync old arguments and register new ones to FunctionArguments
      if(n-- > 0) {
        NamedValues[std::string(Arg.getName())] = &Arg;
        PointerTypes[&Arg] = FunctionArguments[std::string(Arg.getName())].second.first;
        FunctionArguments[std::string(Arg.getName())].first = &Arg;
        continue;
      }

      llvm::Value *ArgSave;
      if(refs[i]) {
        NamedValues[std::string(Arg.getName())] = &Arg;
        PointerTypes[&Arg] = types[i];
        ArgSave = &Arg;
      } else {
        llvm::AllocaInst *Alloca;
        Alloca = CreateEntryBlockAlloca(TheFunction, Arg.getName(), Arg.getType());
        Builder.CreateStore(&Arg, Alloca);
        NamedValues[std::string(Arg.getName())] = Alloca;
        ArgSave = Alloca;
      }
      FunctionArguments[std::string(Arg.getName())] = {ArgSave, {types[i], typesRef[i]}};
      FunctionArgumentStack.push_back(std::string(Arg.getName()));
      ++i;
    }

    auto OldLocals(LocalFunctions);
    LocalFunctions.clear();
    for(Decl *def: *local_def_list) {
      //TODO: add functions to LocalFunctions
      auto v = def->codegen();
      if(v != nullptr && llvm::isa<llvm::Function>(v)) LocalFunctions.push_back((llvm::Function *)v);
      Builder.SetInsertPoint(BB);
    }
    block->codegen();

    //! check for return
    auto lastBB = Builder.GetInsertBlock();
    auto terminator = lastBB->getTerminator();
    if(!terminator/* !llvm::isa<llvm::ReturnInst>(terminator) */) {
      if(TheFunction->getReturnType() == llvm::Type::getVoidTy(TheContext)) Builder.CreateRetVoid();
      else Builder.CreateRet(c32(0));
    }

    llvm::verifyFunction(*TheFunction);

    LocalFunctions = OldLocals;
    NamedValues = OldBindings;
    NamedFunctions = OldFunctions;
    //TODO: restore FunctionArguments state
    FunctionArguments = oldArguments;
    FunctionArgumentStack = oldArgumentStack;
    FunctionStack.pop_back();

    return TheFunction;
  }

private:
  Func_Decl *header;
  std::vector<Decl *> *local_def_list;
  Block *block;
};

class Main_Func: public Func_Decl {
public:
  Main_Func(std::string i, std::vector<Decl *> *p, Type r): Func_Decl(i, p, r) {}
  void analyze() override {
    if(param_list->size()) {
      yyerror("Main/Initial function cannot have any parameters");
    }
    if(ret != NO_t) {
      yyerror("Main/Initial function must return 'nothing'");
    }
  }
  bool isMain() override { return true; }
  llvm::Function* codegen(bool AInst=false) const override {
    llvm::FunctionType *main_type = llvm::FunctionType::get(i32, {}, false);
    llvm::Function *main = llvm::Function::Create(main_type, llvm::Function::ExternalLinkage, "main", TheModule.get());
    return main;
  }
}; /////!!

class Var_Decl: public Decl {
public:
  Var_Decl(std::string i, Type t, bool r, int d, std::vector<int> *ds=nullptr): id(i), type(t), ref(r), dim(d), dim_sizes(ds) {}
  ~Var_Decl() {
    // if(dim_sizes != nullptr) delete dim_sizes;
  }
  void analyze() override {
    if(std::find(names.begin(), names.end(), id) != names.end()) {
      char error_msg[128];
      char *cstr = new char[id.length() + 1];
      strcpy(cstr, id.c_str());
      sprintf(error_msg, "Key token '%s' cannot be used as identifier", cstr);
      delete[] cstr;
      yyerror(error_msg);
    }

    if(dim_sizes) for(auto &x: *dim_sizes) {
      if(x == 0) yyerror("Arrays cannot have size 0");
    }

    auto *entry = st.lookup_quick(id);
    if(entry && entry->type == FUNC_t) {
      char error_msg[128];
      char *cstr = new char[id.length() + 1];
      strcpy(cstr, id.c_str());
      sprintf(error_msg, "'%s' is declared as function and it cannot be declared as variable", cstr);
      delete[] cstr;
      yyerror(error_msg);
    }

    st.insert(id, type, type, dim, ref, dim_sizes);
  }
  void param_insert(std::string func_name) override {
    st.param_insert(func_name, id);
  }
  std::string getId() override { return id; }
  Type getType() override { return type; }
  bool getRef() override { return ref; }
  int getDims() override { return dim; }
  llvm::Value* codegen(bool AInst=false) const override {
    auto var_type = parse_type(type, linear_size());
    auto gVar = new llvm::GlobalVariable(*TheModule, var_type, false, llvm::GlobalValue::ExternalLinkage, llvm::ConstantAggregateZero::get(var_type), id);
    NamedValues[id] = gVar;
    return nullptr;
  }
  int linear_size() const override {
    int res = 1;
    if (dim_sizes != nullptr) for(auto &x: *dim_sizes) {
      if(x == 0) continue;
      res *= x;
    }
    return res;
  }

private:
  std::string id;
  Type type;
  bool ref;
  int dim;
  std::vector<int> *dim_sizes;
};

class Assign: public Stmt {
public:
  Assign(Expr *l, Expr *r): l_val(l), r_val(r) {}
  ~Assign() {
    delete l_val;
    delete r_val;
  }
  void analyze() override {
    l_val->analyze();
    if(l_val->getType() == ARRAY_t || l_val->getType() == STRING_t && l_val->getDim() > 1) {
      yyerror("Cannot assign to array");
    }
    if(l_val->getType() == FUNC_t) {
      yyerror("Cannot assing to function");
    }

    r_val->check_type(l_val->getType(), l_val->getDim());
  }
  llvm::Value* codegen(bool AInst=false) const override {
    llvm::Value *lhs = l_val->codegen(true);
    llvm::Value *rhs = r_val->codegen();
    Builder.CreateStore(rhs, lhs);            //! strings??
    return nullptr;
  }

private:
  Expr *l_val;
  Expr *r_val;
};

class If: public Stmt {
public:
  If(Cond *c, Stmt *t, Stmt *e=nullptr): cond(c), true_stmt(t), else_stmt(e) {}
  ~If() {
    delete cond;
    delete true_stmt;
    delete else_stmt;
  }
  void analyze() override {
    cond->analyze();
    if(true_stmt != nullptr) true_stmt->analyze();
    if(else_stmt != nullptr) else_stmt->analyze();
  }
  llvm::Value* codegen(bool AInst=false) const override {
    llvm::Value *c = cond->codegen();
    //llvm::Value *cond = Builder.CreateICmpNE(v, c32(0), "if_cond");   //? maybe unnecessary.
    llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();

    llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(TheContext, "then", TheFunction);
    llvm::BasicBlock *ElseBB = llvm::BasicBlock::Create(TheContext, "else", TheFunction);
    llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(TheContext, "endif", TheFunction);

    Builder.CreateCondBr(c, ThenBB, ElseBB);
    Builder.SetInsertPoint(ThenBB);
    if(true_stmt != nullptr) true_stmt->codegen();

    Builder.CreateBr(AfterBB);
    Builder.SetInsertPoint(ElseBB);
    if (else_stmt != nullptr) else_stmt->codegen();

    Builder.CreateBr(AfterBB);
    Builder.SetInsertPoint(AfterBB);

    return nullptr;
  }

private:
  Cond *cond;
  Stmt *true_stmt;
  Stmt *else_stmt;
};

class While: public Stmt {
public:
  While(Cond *c, Stmt *s): cond(c), stmt(s) {}
  ~While() {
    delete cond;
    delete stmt;
  }
  void analyze() override {
    cond->analyze();
    stmt->analyze();
  }
  llvm::Value* codegen(bool AInst=false) const override {
    llvm::BasicBlock *PrevBB = Builder.GetInsertBlock();
    llvm::Function *TheFunction = PrevBB->getParent();

    llvm::BasicBlock *LoopBB = llvm::BasicBlock::Create(TheContext, "loop", TheFunction);
    llvm::BasicBlock *BodyBB = llvm::BasicBlock::Create(TheContext, "body", TheFunction);
    llvm::BasicBlock *AfterBB = llvm::BasicBlock::Create(TheContext, "endwhile", TheFunction);

    Builder.CreateBr(LoopBB);
    Builder.SetInsertPoint(LoopBB);
    llvm::Value *loop_cond = cond->codegen();

    Builder.CreateCondBr(loop_cond, BodyBB, AfterBB);
    Builder.SetInsertPoint(BodyBB);

    stmt->codegen();

    Builder.CreateBr(LoopBB);
    Builder.SetInsertPoint(AfterBB);

    return nullptr;
  }

private:
  Cond *cond;
  Stmt *stmt;
};

class Return: public Stmt {
public:
  Return(Type t): type(t) {}
  Return(Expr *e): expr(e) {}
  ~Return() {
    delete expr;
  }
  void analyze() override {
    STEntry *e = st.lookup(FUNC_t);  //! lookup latest function in scope
    if(expr != nullptr) {
      // std::cerr << "Function to return: " << e->id << std::endl;
      expr->check_type(e->ret, 0);
    } else {
      if(e->ret != type) {
        yyerror("Ret mismatch");
      }
    }
  }
  llvm::Value* codegen(bool AInst=false) const override {
    if(expr != nullptr) {
      llvm::Value *v = expr->codegen();
      Builder.CreateRet(v);
    } else {
      llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
      auto Type = TheFunction->getReturnType();
      if(Type == i32) {
        Builder.CreateRet(c32(0));
      } else {
        Builder.CreateRetVoid();
      }
    }

    // llvm::BasicBlock *BB = llvm::BasicBlock::Create(TheContext, "case", TheFunction);
    // Builder.SetInsertPoint(BB);
    return nullptr;
  }

private:
  Type type;
  Expr *expr;
};

class Id: public Expr {
public:
  Id(std::string i, bool a=false): id(i), array(a) {}
  ~Id() {
    for(auto &x: dims) delete x;
  }
  void analyze() override {
    STEntry *e = st.lookup(id);

    if(array && e->type != ARRAY_t && e->type != STRING_t) {
      char error_msg[128];
      char *cstr = new char[id.length() + 1];
      strcpy(cstr, id.c_str());
      sprintf(error_msg, "Identifier '%s' is not declared as array", cstr);
      delete[] cstr;
      yyerror(error_msg);
    }

    if(dims.size() > e->dimenesions) {
      char error_msg[128];
      char *cstr = new char[id.length() + 1];
      strcpy(cstr, id.c_str());
      sprintf(error_msg, "Variable '%s' is declared as array of %d dimension(s), but tried to access %zu dimensions", cstr, e->dimenesions, dims.size());
      delete[] cstr;
      yyerror(error_msg);
    }

    for(auto &x: dims) x->check_type(INT_t, 0);

    if(array) {
      if(e->type == ARRAY_t && dims.size() == e->dimenesions) {
        type = INT_t;
        dimensions = 0;
      } else if(e->type == STRING_t && dims.size() == e->dimenesions) {
        type = CHAR_t;
        dimensions = 0;
      } else {
        type = e->type;
        dimensions = e->dimenesions - dims.size();
      }
    } else {
      type = e->type;
      dimensions = e->dimenesions - dims.size();
    }

    if(((type == ARRAY_t || type == STRING_t) || array) && e->dim_sizes != nullptr) dim_sizes = *(e->dim_sizes);
  }
  void set_array() override {
    array = true;
  }
  void add_expr(Expr *e) override {
    dims.push_back(e);
  }
  bool is_lval() override { return true; }
  llvm::Value* codegen(bool AInst=false) const override {
    auto A = NamedValues[id];
    // std::cerr << id << " :";
    if (array) {
      auto index = calculateArrayIndex();
      // std::cerr << "its array\n";
      
      llvm::Type *tmpType;
      if(llvm::isa<llvm::GlobalVariable>(A)) {
        tmpType = ((llvm::GlobalVariable *)A)->getValueType(); //? array type?
      } else {
        tmpType = PointerTypes[A];
      }

      auto gep = Builder.CreateGEP(tmpType, A, index, id + "_ptr");
      if(AInst) return gep;
      else {
        return Builder.CreateLoad(parse_type(type), gep, id.c_str()); //! type should not be array type
      }
    }
    // std::cerr << "its NOT array\n";
    if(AInst) return A;
    else {
      if(llvm::isa<llvm::AllocaInst>(A))
        return Builder.CreateLoad(((llvm::AllocaInst *)A)->getAllocatedType(), A, id.c_str());
      else if(llvm::isa<llvm::GlobalVariable>(A))
        return Builder.CreateLoad(((llvm::GlobalVariable *)A)->getValueType(), A, id.c_str());
      else {
        return Builder.CreateLoad(PointerTypes[A], A, id.c_str());
      }
    }
  }
  std::string getId() const override {
    return id;
  }

  std::vector<int> getSizes() const override { return dim_sizes; }

private:
  std::string id;
  bool array;
  std::vector<Expr *> dims;
  std::vector<int> dim_sizes;

  std::vector<llvm::Value*> calculateArrayIndex() const {
    std::vector<llvm::Value*> indices;
    for(auto &e: dims) {
      auto dimValue = e->codegen();
      indices.push_back(dimValue);
    }

    auto linearIndex = calculateLinearIndex(indices);

    std::vector<llvm::Value*> gepIndices;
    gepIndices.push_back(c32(0));
    gepIndices.push_back(linearIndex);

    return gepIndices;
  }

  llvm::Value* calculateLinearIndex(const std::vector<llvm::Value*>& indices) const {
    auto it_sizes = dim_sizes.rbegin();
    llvm::Value *idx=c32(0), *next_dim_size=c32(*it_sizes), *dim_size=c32(1);

    for(auto it=indices.rbegin(); it!=indices.rend(); ++it) {
      auto tmp = Builder.CreateMul(*it, dim_size);
      idx = Builder.CreateAdd(idx, tmp);
      dim_size = Builder.CreateMul(dim_size, next_dim_size);
      next_dim_size = c32(*(++it_sizes));
    }

    return idx;
  }
};

class IntConst: public Expr {
public:
  IntConst(int v): val(v) {}
  void analyze() override {
    type = INT_t;
    dimensions = 0;
  }
  llvm::Value* codegen(bool AInst=false) const override {
    return c32(val);
  }

private:
  int val;
};

class CharConst: public Expr {
public:
  CharConst(const char *v): val(v) {}
  void analyze() override {
    type = CHAR_t;
    dimensions = 0;
  }
  llvm::Value* codegen(bool AInst=false) const override {
    char res;
    auto sub = val;

    if(sub[0] == '\\') {
      if(sub[1] == 'x') {
        auto str_hex = sub.substr(2);
        auto i_hex = std::stoi(str_hex, nullptr, 16);
        res = (char)i_hex;
      } else {
        switch(sub[1]) {
          case 'n': res = '\n'; break;
          case 't': res = '\t'; break;
          case 'r': res = '\r'; break;
          case '0': res = '\0'; break;
          default: res = sub[1]; break;
        }
      }
    } else {
      res = sub[0];
    }

    return c8(res);
  }

private:
  std::string val;
};

class StringConst: public Expr {
public:
  StringConst(std::string v): val(v) {}
  void analyze() override {
    type = STRING_t;
    dimensions = 1;
  }
  bool is_lval() override { return true; }
  llvm::Value* codegen(bool AInst=false) const override {
    bool escape = false;
    bool escape_hex = false;
    std::string hex;
    std::vector<llvm::Constant *> str_arr;

    for(auto &c: val) {
      if(escape_hex) {
        if(hex.size() < 2) {
          hex.push_back(c);
          continue;
        }
        auto i_hex = std::stoi(hex, nullptr, 16);
        str_arr.push_back(c8((char)i_hex));
        escape_hex = false;
        hex.clear();
      } 
      if(escape) {
        escape = false;
        switch(c) {
          case 'n': str_arr.push_back(c8('\n')); break;        
          case 'r': str_arr.push_back(c8('\r')); break;        
          case 't': str_arr.push_back(c8('\t')); break;        
          case '0': str_arr.push_back(c8('\0')); break;        
          case 'x': escape_hex = true; break;        
          default: str_arr.push_back(c8(c)); break;
        }
      } else if(c == '\\') {
        escape = true;
      } else {
        str_arr.push_back(c8(c));
      }
    }
    str_arr.push_back(c8('\0'));

    auto str_type = llvm::ArrayType::get(i8, str_arr.size());
    auto constStr = llvm::ConstantArray::get(str_type, str_arr);
    if(AInst) {
      auto var_type = llvm::ArrayType::get(i8, str_arr.size());
      auto gVar = new llvm::GlobalVariable(*TheModule, var_type, true, llvm::GlobalValue::PrivateLinkage, constStr, "constStr");
      return gVar;
    }
    else
      return constStr;
  }

private:
  std::string val;
};

class SignedInt: public Expr {
public:
  SignedInt(const char* s, Expr *e): expr(e), sign(s) {}
  ~SignedInt() {
    delete expr;
  }
  void analyze() override {
    expr->check_type(INT_t, 0);
    type = INT_t;
    dimensions = 0;
  }
  llvm::Value* codegen(bool AInst=false) const override {
    llvm::Value *v = expr->codegen();

    if(sign == "-") {
      v = Builder.CreateMul(v, c32(-1), "negtmp");
    }

    return v;
  }

private:
  Expr *expr;
  std::string sign;
};

class BinOp: public Expr {
public:
  BinOp(Expr *e1, const char* o, Expr *e2): expr1(e1), op(o), expr2(e2) {}
  ~BinOp() {
    delete expr1;
    delete expr2;
  }
  void analyze() override {
    expr1->check_type(INT_t, 0);
    expr2->check_type(INT_t, 0);
    type = INT_t;
    dimensions = 0;
  }
  llvm::Value* codegen(bool AInst=false) const override {
    llvm::Value* l = expr1->codegen();
    llvm::Value* r = expr2->codegen();

    if(op == "+") return Builder.CreateAdd(l, r, "addtmp");
    if(op == "-") return Builder.CreateSub(l, r, "subtmp");
    if(op == "*") return Builder.CreateMul(l, r, "multmp");
    if(op == "div") return Builder.CreateSDiv(l, r, "divtmp");
    if(op == "mod") return Builder.CreateSRem(l, r, "modtmp");

    return nullptr;
  }

private:
  Expr *expr1;
  std::string op;
  Expr *expr2;
};

class Func_Call: public Expr, public Stmt {
public:
  Func_Call(std::string i, std::vector<Expr *> *p): id(i), params(p) {}
  ~Func_Call() {
    if(params) for(auto &e: *params) delete e;
    delete params;
  }
  void analyze() override {
    STEntry *func = st.lookup(id);

    if(func->type != FUNC_t) {
      char error_msg[128];
      char *cstr = new char[id.length() + 1];
      strcpy(cstr, id.c_str());
      sprintf(error_msg, "'%s' not declared as function", cstr);
      delete[] cstr;
      yyerror(error_msg);
    }

    if(!func->defined && std::find(rt_funcs.begin(), rt_funcs.end(), id) == rt_funcs.end()) {
      char error_msg[128];
      char *cstr = new char[id.length() + 1];
      strcpy(cstr, id.c_str());
      sprintf(error_msg, "Function '%s()' used but no definition found", cstr);
      delete [] cstr;
      yyerror(error_msg);
    }

    auto st_params = st.get_params(id);
    auto st_params_length = st_params.size();

    auto params_length = params ? params->size() : 0;
    if(params_length != st_params_length) {
      char error_msg[128];
      char *cstr = new char[id.length() + 1];
      strcpy(cstr, id.c_str());

      sprintf(error_msg, "In function '%s()', expected %lu paramater(s) but got %lu", cstr, st_params_length, params_length);

      delete[] cstr;
      yyerror(error_msg);
    }

    if(params_length) {
      auto it_p = params->begin();
      auto it_stp = st_params.begin();
      for(int par_count = 1; it_p!=params->end() && it_stp!=st_params.end(); ++it_p, ++it_stp, ++par_count) {
        if(it_stp->dim_sizes)
          (*it_p)->check_type(it_stp->type, it_stp->dimenesions, *it_stp->dim_sizes);
        else 
          (*it_p)->check_type(it_stp->type, it_stp->dimenesions);
        
        if(it_stp->ref && !(*it_p)->is_lval()) {
          char error_msg[128];
          char *cstr = new char[id.length() + 1];
          strcpy(cstr, id.c_str());
          sprintf(error_msg, "In function call '%s()', parameter %d is not lval", cstr, par_count);
          delete[] cstr;
          yyerror(error_msg);
        }
      }
    }

    type = func->ret;
    dimensions = 0;
  }
  llvm::Value* codegen(bool AInst=false) const override {     ///// save old bindings and restore
    auto CalleeF = NamedFunctions[id].first;
    auto refs = NamedFunctions[id].second;

    if(!CalleeF) {
      if(id == "writeInteger") {
        auto n = (*params)[0]->codegen();
        auto n64 = Builder.CreateSExt(n, i64, "ext");
        return Builder.CreateCall(TheWriteInteger, {n64});
      }

      if(id == "readString") {
        auto n = (*params)[0]->codegen();
        auto s = (*params)[1]->codegen(true);
        auto n64 = Builder.CreateSExt(n, i64, "ext");
        return Builder.CreateCall(TheReadString, {n64, s});
      }

      if(id == "chr") {
        auto n = (*params)[0]->codegen();
        auto n64 = Builder.CreateSExt(n, i64, "ext");
        return Builder.CreateCall(TheChr, {n64});
      }

      CalleeF = TheModule->getFunction(id);
    }

    unsigned i = 0;
    std::vector<llvm::Value *> ArgsV;

    //TODO: add arguments from FunctionArguments
    //* check which arguments are needed from FunctionArguments
    //* lookup arguments from NamedValues and pass them
    //! argumentss MUST be passed by ref [codegen(true)]
    //? check if function in functionStack
    bool found = false;
    // std::cerr << "FunctionStack:\n";
    for(auto &x: FunctionStack) {
      if(x == CalleeF) found = true;
      // std::cerr << (std::string)x->getName() << std::endl;
    }
    // std::cerr << "\nLocalFunctions:\n";
    for(auto &x: LocalFunctions) {
      if(x == CalleeF) found = true;
      // std::cerr << (std::string)x->getName() << std::endl;
    }
    // std::cerr << "\n";
    
    if(found) {
      auto requiredArgs = CalleeF->arg_size();
      signed n = requiredArgs - params->size();
      // std::cerr << "N. Args: " << "req=" << requiredArgs << " input=" << params->size() << std::endl;
      // std::cerr << "\n\n\n";
      /* for(auto &x: FunctionArguments) {
        if(n-- > 0) {
          ArgsV.push_back(x.second.first);
        }
      } */
      for(auto &x: FunctionArgumentStack) {
        if(n-- > 0) {
          ArgsV.push_back(FunctionArguments[x].first);
        }
      }
    }

    if(params != nullptr) for(Expr *arg: *params) {
      ArgsV.push_back(arg->codegen(refs[i++]));
    }

    if(CalleeF->getReturnType()->isVoidTy())
      return Builder.CreateCall(CalleeF, ArgsV);
    else
      return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
  }

private:
  std::string id;
  std::vector<Expr *> *params;
};


class NotCond: public Cond {
public:
  NotCond(Cond *c): cond(c) {}
  ~NotCond() {
    delete cond;
  }
  void analyze() override {
    cond->analyze();
  }
  llvm::Value* codegen(bool AInst=false) const override {
    llvm::Value* v = cond->codegen();
    return Builder.CreateNot(v, "nottmp");
  }

private:
  Cond *cond;
};

class BinCond: public Cond {
public:
  BinCond(Cond *c1, const char* o, Cond *c2): cond1(c1), cond2(c2), op(o) {}
  ~BinCond() {
    delete cond1;
    delete cond2;
  }
  void analyze() override {
    cond1->analyze();
    cond2->analyze();
  }
  llvm::Value* codegen(bool AInst=false) const override {
    llvm::Value* l = cond1->codegen();
    llvm::Value* r = cond2->codegen();

    if(op == "and") {
      return Builder.CreateAnd(l, r, "andtmp");
    }



    if(op == "or") return Builder.CreateOr(l, r, "ortmp");

    return nullptr;
  }

private:
  Cond *cond1;
  std::string op;
  Cond *cond2;
};

class BinOpCond: public Cond {
public:
  BinOpCond(Expr *e1, const char* o, Expr *e2): expr1(e1), expr2(e2), op(o) {}
  ~BinOpCond() {
    delete expr1;
    delete expr2;
  }
  void analyze() override {
    expr1->check_type_cond();
    expr2->check_type(expr1->getType(), 0);
  }
  llvm::Value* codegen(bool AInst=false) const override {
    llvm::Value* l = expr1->codegen();
    llvm::Value* r = expr2->codegen();

    if(op == "<") return Builder.CreateICmpSLT(l, r, "lttmp");
    if(op == ">") return Builder.CreateICmpSGT(l, r, "gttmp");
    if(op == "<=") return Builder.CreateICmpSLE(l, r, "ltetmp");
    if(op == ">=") return Builder.CreateICmpSGE(l, r, "gtetmp");
    if(op == "=") return Builder.CreateICmpEQ(l, r, "eqtmp");
    if(op == "#") return Builder.CreateICmpNE(l, r, "netmp");

    return nullptr;
  }

private:
  Expr *expr1;
  std::string op;
  Expr *expr2;
};

#endif
