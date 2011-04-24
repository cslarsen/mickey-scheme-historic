#ifndef INC_MICKEY_CONS_H
#define INC_MICKEY_CONS_H

#include <string>
#include <map>
#include "util.h"

// types taken from the PICOBIT scheme paper

enum type_t {
  NIL,
  INTEGER,
  CLOSURE,
  PAIR,
  SYMBOL,
  STRING,
  U8VECTOR,
  CONTINUATION
};

typedef struct cons_t* (*lambda_t)(struct cons_t*, struct environment_t*);
typedef std::map<std::string, struct cons_t*> dict_t; // TODO: Use hash_map

struct environment_t
{
  struct environment_t *outer;
  dict_t symbols;

  environment_t() : outer(NULL)
  {
  }

  struct cons_t* lookup(const std::string& name) const;
  struct symbol_t* create_symbol(const std::string& name);
  void defun(const std::string& name, lambda_t func);
};

struct continuation_t
{
};

struct closure_t
{
  lambda_t function;
  environment_t* environment;
};

struct u8vector_t
{
};

class symbol_t
{
  symbol_t(const char* s) : name(s)
  {
  }

  symbol_t()
  {
  }

  friend struct environment_t;

public:
  std::string name;
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

// Should be in util, but cannot due to circular
// dependencies between cons.h and util.h (TODO: Fix that)
std::string encode_str(const char*);
std::string to_s(enum type_t);

#endif
