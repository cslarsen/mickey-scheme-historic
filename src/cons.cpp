/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>
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
#include "options.h"
#include "eval.h"
#include "exceptions.h"

static std::map<std::string, symbol_t> symbols;

const symbol_t* create_symbol(const std::string& s)
{
  if ( s.empty() )
    raise(std::runtime_error("Symbols must have names"));

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
  case NIL:          return "nil";          break;
  case BOOLEAN:      return "boolean";      break;
  case CHAR:         return "char";         break;
  case DECIMAL:      return "decimal";      break;
  case INTEGER:      return "integer";      break;
  case CLOSURE:      return "closure";      break;
  case PAIR:         return "pair";         break;
  case SYMBOL:       return "symbol";       break;
  case SYNTAX:       return "syntax";       break;
  case STRING:       return "string";       break;
  case VECTOR:       return "vector";       break;
  case CONTINUATION: return "continuation"; break;
  case BYTEVECTOR:   return "bytevector";   break;
  case PORT:         return "port";         break;
  }

  return "#<unknown type>";
}

std::string to_s(cons_t *p)
{
  switch ( type_of(p) ) {
  case NIL:      return "#<nil>";
  case BOOLEAN:  return to_s(p->boolean);
  case CHAR:     return to_s(p->character, false);
  case DECIMAL:  return to_s(p->decimal);
  case INTEGER:  return to_s(p->integer);
  case CLOSURE:  return format("#<closure %p>", p->closure);
  case PAIR:     return to_s(car(p)) + " . " + to_s(cdr(p));
  case SYMBOL:   return p->symbol->name();
  case SYNTAX:   return format("#<syntax_transformer %p>", p->syntax);
  case STRING:   return p->string;
  case VECTOR:   return format("#<vector %p>", p->vector);
  case CONTINUATION: return format("#<continuation %p>", p->continuation);
  case BYTEVECTOR:   return format("#<bytevector %p>", p->bytevector);
  case PORT:     return format("#<port %p>", p->port);
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

std::string to_s(port_t* p)
{
  return format("#<port %p>", p);
}

std::string to_s(char p, bool escape)
{
  return format(escape? "#\\%c" : "%c", isprint(p)? p : '?' );
}

cons_t* environment_t::lookup_or_throw(const std::string& name) const
{
  cons_t *p = lookup(name);

  if ( p == NULL )
    raise(std::runtime_error(format("Unbound definition: %s", name.c_str())));

  return p;
}

cons_t* environment_t::lookup(const std::string& name) const
{
  const environment_t *e = this;
  dict_t::const_iterator i;

  do {
    if ( (i = e->symbols.find(name)) != e->symbols.end() )
      return (*i).second;

  } while ( (e = e->outer) != NULL);

  return NULL;
}

struct cons_t* environment_t::define(const std::string& name, lambda_t f)
{
  if ( global_opts.verbose && symbols.find(name) != symbols.end() )
    fprintf(stderr, "WARNING: Already have a definition for %s\n",
        name.c_str());

  symbols[name] = closure(f, this);
  return symbols[name];
}

struct cons_t* environment_t::define(const std::string& name, cons_t* body)
{
  if ( global_opts.verbose && symbols.find(name) != symbols.end() )
    fprintf(stderr, "WARNING: Already have a definition for %s\n",
        name.c_str());

  return symbols[name] = body;
}

struct environment_t* environment_t::extend()
{
  environment_t *r = new environment_t();
  r->outer = this;
  return r;
}

environment_t* environment_t::outmost()
{
  environment_t *e = this;

  while ( e->outer != NULL )
    e = e->outer;

  return e;
}

cons_t* deep_copy(const cons_t *p)
{
  if ( !p )
    return NULL;

  cons_t *r = new cons_t();
  memcpy(r, p, sizeof(cons_t));

  if ( listp(r) ) {
    r->car = deep_copy(r->car);
    r->cdr = deep_copy(r->cdr);
  } else if ( syntaxp(r) )
    r->syntax->transformer = deep_copy(r->syntax->transformer);
  else if ( stringp(r) )
    r->string = copy_str(r->string);

  return r;
}

int merge(environment_t *to, const environment_t *from)
{
  int r = 0;

  for ( dict_t::const_iterator i = from->symbols.begin();
        i != from->symbols.end(); ++i )
  {
    std::string name = (*i).first;

    // copy binding
    if ( to->lookup(name) == NULL ) {
      to->symbols[name] = (*i).second;
      ++r;
      continue;
    }

    // skip special symbol `import´
    if ( name != "import" ) {
      if ( global_opts.warn ) {
        fprintf(stderr,
            "WARNING: We already have a definition for: %s\n",
             name.c_str());
      }

      // TODO: Allow overrides in REPL-environments
      //raise(runtime_exception("Binding already exists: " + name));
      //break;
    }
  }

  return r;
}
