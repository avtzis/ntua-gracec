#ifndef __SYMBOL_HPP__
#define __SYMBOL_HPP__

#include <map>
#include <vector>
#include <string>
#include <cstring>
#include <cstdio>

extern void yyerror(const char *msg);

enum Type { INT_t, CHAR_t, ARRAY_t, STRING_t, FUNC_t, NO_t };

struct STEntry {
  Type type, ret;
  int dimenesions;
  std::string id;
  bool ref, defined;
  std::vector<int> *dim_sizes;
  STEntry() {}
  STEntry(Type t, Type r, int d, std::string i, bool re, std::vector<int> *ds, bool de=false): type(t), ret(r), dimenesions(d), id(i), ref(re), dim_sizes(ds), defined(de) {}
};

class Scope {
public:
  Scope() {}
  STEntry *lookup(std::string id) {
    if(locals.find(id) == locals.end()) return nullptr;
    return &(locals[id]);
  }
  STEntry *lookup(Type type) {
    for(auto it=locals_stack.rbegin(); it!=locals_stack.rend(); ++it) {
      auto entry = *it;
      if(entry.type == type) return &(*it);
    }
    return nullptr;
  }
  void insert(std::string id, Type type, Type ret, int dim, bool ref, std::vector<int> *dim_sizes, bool defined=false) {
    if(locals.find(id) != locals.end()) {
      char error_msg[128];                         
      char *cstr = new char[id.length() + 1];
      strcpy(cstr, id.c_str());
      sprintf(error_msg, "Duplicate variable declaration: '%s'", cstr);
      delete[] cstr;
      yyerror(error_msg);
    }
    auto e = STEntry(type, ret, dim, id, ref, dim_sizes, defined);
    locals[id] = e;
    locals_stack.push_back(e);
    if(type == FUNC_t) ft[id] = std::vector<STEntry>();
  }
  void param_insert(std::string func_name, STEntry param_entry) {
    ft[func_name].push_back(param_entry);
  }
  auto get_params(std::string func_name) {
    return ft[func_name];
  }

private:
  std::map<std::string, STEntry> locals;
  std::vector<STEntry> locals_stack;
  std::map<std::string, std::vector<STEntry>> ft;
};

class SymbolTable {
public: 
  STEntry *lookup(std::string id) {
    for(auto s=scopes.rbegin(); s!=scopes.rend(); ++s) {
      STEntry *e = s->lookup(id);
      if(e != nullptr) return e;
    }
    char error_msg[128];
    char *cstr = new char[id.length() + 1];
    strcpy(cstr, id.c_str());
    sprintf(error_msg, "Variable '%s' has not been declared", cstr);
    delete[] cstr;
    yyerror(error_msg);

    return nullptr;
  }
  STEntry *lookup_quick(std::string id) {
    for(auto s=scopes.rbegin(); s!=scopes.rend(); ++s) {
      STEntry *e = s->lookup(id);
      if(e != nullptr) return e;
    }
    return nullptr;
  }
  STEntry *lookup_current_scope(std::string id) {
    STEntry *e = scopes.back().lookup(id);
    if(e != nullptr) return e;
    return nullptr;
  }
  STEntry *lookup_last_scope(std::string id) {
    auto it = ++scopes.rbegin();
    STEntry *e = it->lookup(id);
    if(e != nullptr) return e;

    char error_msg[128];
    char *cstr = new char[id.length() + 1];
    strcpy(cstr, id.c_str());
    sprintf(error_msg, "Variable '%s' has not been declared in current scope", cstr);
    delete[] cstr;
    yyerror(error_msg);

    return nullptr;
  }
  STEntry *lookup(Type type) {
    auto s = ++scopes.rbegin();
    STEntry *e = s->lookup(type);
    if(e == nullptr) yyerror("Function not found");
    return e;
  }
  auto lookup_scope(std::string id) {
    for(auto s=scopes.rbegin(); s!=scopes.rend(); ++s) {
      STEntry *e = s->lookup(id);
      if(e != nullptr) return s;
    }
    char error_msg[128];
    char *cstr = new char[id.length() + 1];
    strcpy(cstr, id.c_str());
    sprintf(error_msg, "Identifier '%s' has not been declared", cstr);
    delete[] cstr;
    yyerror(error_msg);

    return scopes.rend();
  }
  void insert(std::string id, Type type, Type ret, int dim, bool ref, std::vector<int> *dim_sizes, bool defined=false) {
    scopes.back().insert(id, type, ret, dim, ref, dim_sizes, defined);
  }
  void param_insert(std::string func_name, std::string id) {
    auto func_entry_scope = lookup_scope(func_name);
    STEntry *param_entry = scopes.back().lookup(id);
    func_entry_scope->param_insert(func_name, *param_entry);
  }
  void push_scope() {
    scopes.push_back(Scope());
  }
  void pop_scope() {
    scopes.pop_back();
  }

  auto get_params(std::string func_name) {
    auto func_entry_scope = lookup_scope(func_name);
    return func_entry_scope->get_params(func_name);
  }

private:
  std::vector<Scope> scopes;
};

extern SymbolTable st;

#endif
