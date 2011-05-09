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

#include <stdexcept>
#include "cons.h"
#include "util.h"
#include "primops.h"
#include "print.h"

static std::map<std::string, symbol_t> symbols;

const symbol_t* create_symbol(const std::string& s)
{
  if ( s.empty() )
    throw std::runtime_error("Symbols must have names");

  std::map<std::string, symbol_t>::iterator i;

  if ( (i = symbols.find(s)) == symbols.end() ) {
    symbols[s] = symbol_t();
    i = symbols.find(s);
    (*i).second.n = &((*i).first);
  }

  return &(*i).second;
}

std::string to_s(enum type_t type)
{
  switch ( type ) {
  case NIL:          return "nil";      break;
  case BOOLEAN:      return "boolean";  break;
  case CHAR:         return "char";     break;
  case DECIMAL:      return "decimal";  break;
  case INTEGER:      return "integer";  break;
  case CLOSURE:      return "closure";  break;
  case PAIR:         return "pair";     break;
  case SYMBOL:       return "symbol";   break;
  case STRING:       return "string";   break;
  case VECTOR:       return "vector"; break;
  case CONTINUATION: return "continuation"; break;
  }

  return "#<unknown type>";
}

std::string to_s(cons_t *p)
{
  switch ( type_of(p) ) {
  case NIL:      return "#<nil>";
  case BOOLEAN:  return to_s_bool(p->boolean);
  case CHAR:     return to_s(p->character, false);
  case DECIMAL:  return to_s_float(p->decimal);
  case INTEGER:  return to_s_int(p->integer);
  case CLOSURE:  return format("#<closure %p>", p->closure);
  case PAIR:     return to_s(car(p)) + " . " + to_s(cdr(p));
  case SYMBOL:   return p->symbol->name();
  case STRING:   return p->string;
  case VECTOR:   return format("#<vector %p>", p->vector);
  case CONTINUATION: return format("#<continuation %p>", p->continuation);
  }

  return "#<unknown type>";
}

std::string to_s(closure_t* p)
{
  return format("#<closure %p>", p);
}

std::string to_s(continuation_t* p)
{
  return format("#<continuation %p>", p);
}

std::string to_s(vector_t* p)
{
  return format("#<vector %p>", p);
}

std::string to_s(char p, bool escape)
{
  // TODO: Use table instead
  return format(escape? "#\\%c" : "%c", (p>32 || p<127)? p : '?' );
}

cons_t* environment_t::lookup_or_throw(const std::string& name) const
{
  cons_t *p = lookup(name);

  if ( p == NULL )
    throw std::runtime_error("Unbound variable: " + name);

  return p;
}

cons_t* environment_t::lookup(const std::string& name) const
{
  const environment_t *e = this;

  do {
    dict_t::const_iterator i;

    if ( (i = e->symbols.find(name)) != e->symbols.end() )
      return (*i).second;

  } while ( (e = e->outer) != NULL);

  return NULL;
}

struct cons_t* environment_t::define(const std::string& name, lambda_t f)
{
  symbols[name] = closure(f, this);
  return symbols[name];
}

struct cons_t* environment_t::define(const std::string& name, cons_t* body)
{
  // TODO: Perform deep copy?  Extend environment?
  return symbols[name] = body;
}

struct environment_t* environment_t::extend()
{
  environment_t *r = new environment_t();
  r->outer = this;
  return r;
}
