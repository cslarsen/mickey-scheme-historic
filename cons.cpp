#include <stdexcept>
#include "cons.h"
#include "util.h"
#include "primops.h"
#include "print.h"

std::string to_s(enum type_t type)
{
  switch ( type ) {
  case NIL:          return "nil";      break;
  case BOOLEAN:      return "boolean";  break;
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
  case BOOLEAN:  return to_s(p->boolean);
  case INTEGER:  return to_s(p->integer);
  case CLOSURE:  return format("#<closure %p>", p->closure);
  case PAIR:     return to_s(car(p)) + " . " + to_s(cdr(p));
  case SYMBOL:   return p->symbol->name;
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

cons_t* environment_t::create_symbol(const std::string& name)
{
  cons_t *p = lookup(name);

  if ( nullp(p) ) {
    p = new cons_t();
    p->type = SYMBOL;
    p->symbol = new symbol_t(name.c_str());
    symbols[name] = p;
  }

  return p;
}

void environment_t::defun(const std::string& name, lambda_t f)
{
  symbols[name] = closure(f, this);
}

struct cons_t* environment_t::define(const std::string& name, cons_t* body)
{
  // TODO: Perform deep copy?  Extend environment?
  return symbols[name] = body;
}
