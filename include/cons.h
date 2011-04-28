#ifndef INC_MICKEY_CONS_H
#define INC_MICKEY_CONS_H

#include <stdexcept>
#include <string>
#include <map>
#include "util.h"
#include "heap.h"

// types taken from the PICOBIT scheme paper

enum type_t {
  NIL,
  BOOLEAN,
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

struct environment_t : public gc
{
  struct environment_t *outer;
  dict_t symbols;

  environment_t() : outer(NULL)
  {
  }

  struct cons_t* lookup(const std::string& name) const;
  struct cons_t* create_symbol(const std::string& name);
  void defun(const std::string& name, lambda_t func);
  struct cons_t* define(const std::string& name, cons_t* body);
};

struct continuation_t : public gc
{
};

struct closure_t : public gc
{
  lambda_t function;
  environment_t* environment;
};

struct u8vector_t : public gc
{
};

class symbol_t : public gc
{
  symbol_t(); // require param

  symbol_t(const char* s) : name(s? s : "")
  {
    if ( name.empty() )
      throw std::runtime_error("symbol_t() constructed with empty name");
  }

  friend struct environment_t;

public:
  std::string name;
};

struct cons_t : public gc {
  type_t type;
  union {
    bool boolean;
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
