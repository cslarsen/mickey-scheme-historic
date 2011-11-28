/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the modified BSD license.            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

typedef double decimal_t;

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
  CHAR,
  INTEGER,
  DECIMAL,
  CLOSURE,
  PAIR,
  SYMBOL,
  STRING,
  VECTOR,
  CONTINUATION,
  SYNTAX
};

typedef struct cons_t* (*lambda_t)(struct cons_t*, struct environment_t*);
typedef std::map<std::string, struct cons_t*> dict_t; // TODO: Use hash_map

struct environment_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
  struct environment_t *outer;
  dict_t symbols;

  environment_t() : outer(NULL)
  {
  }

  environment_t* extend();
  struct cons_t* lookup(const std::string& name) const;
  struct cons_t* lookup_or_throw(const std::string& name) const;
  struct cons_t* define(const std::string& name, lambda_t func);
  struct cons_t* define(const std::string& name, cons_t* body);
};

struct continuation_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
};

struct closure_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
  lambda_t function;
  environment_t* environment;
};

struct syntax_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
  cons_t* transformer;
  environment_t* environment;
};

struct vector_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
};

class symbol_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
public:
  const std::string *n;

  symbol_t() : n(NULL)
  {
  }

  symbol_t(const symbol_t& s) : n(s.n)
  {
  }

  const std::string& name() const
  {
    return *n;
  }
};

const symbol_t* create_symbol(const std::string& s);

/*
 * TODO: To cons_t, Add `marked` (for GC) and `mutable/immutable` (per spec)
 */

struct cons_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
  type_t type;
  union {
    bool boolean;
    char character;
    int integer;
    decimal_t decimal;
    struct { cons_t *car, *cdr; }; // pair
    closure_t* closure;
    syntax_t* syntax;
    const symbol_t* symbol;
    const char* string;
    vector_t* vector;
    continuation_t* continuation;
  };
};

// Should be in util, but cannot due to circular
// dependencies between cons.h and util.h (TODO: Fix that)
std::string encode_str(const char*);
std::string to_s(enum type_t);
std::string to_s(closure_t*);
std::string to_s(continuation_t*);
std::string to_s(vector_t*);
std::string to_s(char, bool);
std::string to_s(struct cons_t *p);;

#endif
