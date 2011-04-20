#ifndef INC_MICKEY_CONS_H
#define INC_MICKEY_CONS_H

#include <string>
#include <map>
#include "util.h"

enum type_t {
  NIL, INTEGER, CLOSURE, PAIR, SYMBOL, STRING, U8VECTOR, CONTINUATION
};

typedef struct cons_t* (*lambda_t)(struct cons_t*);

struct environment_t {
    struct environment_t *outer;
    std::map<std::string, struct symbol_t*> symbols;

    environment_t() : outer(NULL)
    {
    }
};

struct continuation_t {
};

struct closure_t {
  lambda_t function;
  environment_t* environment;
};

struct u8vector_t {
};

class symbol_t {
  symbol_t(const char* s) : name(s)
  {
  }

  symbol_t()
  {
  }

public:
  std::string name;

  static symbol_t* create_symbol(const char* s, environment_t* env)
  {
    std::string S = toupper(s);

    if ( env->symbols.find(S) == env->symbols.end() )
      env->symbols[S] = new symbol_t(S.c_str());

    return env->symbols[S];
   }
};

struct cons_t {
  type_t type;
  union {
    int integer;
    struct { cons_t *car, *cdr; }; // pair
    closure_t* closure;
    symbol_t* symbol;
    const char* string;
    u8vector_t* u8vector;
    continuation_t* continuation;
  };
};

#endif
