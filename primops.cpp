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

#include <stdint.h> // limits
#include <math.h> // floor
#include <stdexcept>
#include "primops.h"
#include "util.h"
#include "assertions.h"
#include "print.h"
#include "exceptions.h"

cons_t* cons(const cons_t* head, const cons_t* tail)
{
  cons_t *p = new cons_t();
  p->type = PAIR;
  p->car = const_cast<cons_t*>(head);
  p->cdr = const_cast<cons_t*>(tail);
  return p;
}

cons_t* list(const cons_t* head, const cons_t* tail)
{
  if ( tail == NULL )
    return cons(head, tail);
  else
    return cons(head, cons(tail, NULL));
}

cons_t* symbol(const char* s, environment_t*)
{
  cons_t *p = new cons_t();
  p->type = SYMBOL;
  p->symbol = create_symbol(s);
  return p;
}

cons_t* nil()
{
  cons_t *p = new cons_t();
  p->type = NIL;
  return p;
}

cons_t* integer(int n)
{
  cons_t *p = new cons_t();
  p->type = INTEGER;
  p->integer = n;
  return p;
}

cons_t* decimal(decimal_t n)
{
  cons_t *p = new cons_t();
  p->type = DECIMAL;
  p->decimal = n;
  return p;
}

cons_t* boolean(bool f)
{
  cons_t *p = new cons_t();
  p->type = BOOLEAN;
  p->boolean = f;
  return p;
}

cons_t* character(char c)
{
  cons_t *p = new cons_t();
  p->type = CHAR;
  p->character = c;
  return p;
}

cons_t* string(const char* s)
{
  cons_t *p = new cons_t();
  p->type = STRING;
  p->string = copy_str(s);
  return p;
}

cons_t* vector(cons_t* p, size_t size, cons_t* fill)
{
  vector_t *v;

  if ( size )
    v = fill? new vector_t(size, fill) : new vector_t(size, nil());
  else {
    v = new vector_t();

    while ( !nullp(p) ) {
      v->vector.push_back(car(p));
     p = cdr(p);
    }
  }

  cons_t *r = new cons_t();
  r->type = VECTOR;
  r->vector = v;
  return r;
}

cons_t* car(const cons_t* p)
{
  return ( p != NULL && p->type == PAIR ) ? p->car : NULL;
}

cons_t* cdr(const cons_t* p)
{
  return ( p != NULL && p->type == PAIR ) ? p->cdr : NULL;
}

cons_t* caar(const cons_t* p)
{
  return car(car(p));
}

cons_t* caaar(const cons_t* p)
{
  return car(caar(p));
}

cons_t* cadr(const cons_t* p)
{
  return car(cdr(p));
}

cons_t* caadr(const cons_t* p)
{
  return car(cadr(p));
}

cons_t* caddr(const cons_t* p)
{
  return car(cddr(p));
}

cons_t* cddr(const cons_t* p)
{
  return cdr(cdr(p));
}

cons_t* cdddr(const cons_t* p)
{
  return cdr(cdr(cdr(p)));
}

cons_t* cdaddr(const cons_t* p)
{
  return cdr(car(cdr(cdr(p))));
}

cons_t* caaddr(const cons_t* p)
{
  return car(car(cdr(cdr(p))));
}

cons_t* cdar(const cons_t* p)
{
  return cdr(car(p));
}

cons_t* cadar(const cons_t* p)
{
  return car(cdar(p));
}

cons_t* caddar(const cons_t* p)
{
  return car(cdr(cdar(p)));
}

cons_t* cadaar(const cons_t* p)
{
  return car(cdr(car(car(p))));
}

type_t type_of(const cons_t* p)
{
  return p == NULL ? NIL : p->type;
}

bool symbolp(const cons_t* p)
{
  return type_of(p) == SYMBOL;
}

bool atomp(const cons_t* p)
{
  return !pairp(p); // Queinnec, p. 4
}

bool integerp(const cons_t* p)
{
  return type_of(p) == INTEGER;
}

bool decimalp(const cons_t* p)
{
  return type_of(p) == DECIMAL;
}

bool vectorp(const cons_t* p)
{
  return type_of(p) == VECTOR;
}

bool charp(const cons_t* p)
{
  return type_of(p) == CHAR;
}

bool booleanp(const cons_t* p)
{
  return type_of(p) == BOOLEAN;
}

bool numberp(const cons_t* p)
{
  return integerp(p) || decimalp(p);
}

bool stringp(const cons_t* p)
{
  return type_of(p) == STRING;
}

bool nullp(const cons_t* p)
{
  return type_of(p) == NIL
    || (type_of(p)==PAIR && type_of(car(p))==NIL && type_of(cdr(p))==NIL);
}

bool pairp(const cons_t* p)
{
  // (1) A pair is a list, (2) except for the empty list '()
  return listp(p) &&                   // (1)
    !(nullp(car(p)) && nullp(cdr(p))); // (2)
}

bool listp(const cons_t* p)
{
  return type_of(p) == PAIR;
}

bool closurep(const cons_t* p)
{
  return type_of(p) == CLOSURE;
}

bool syntaxp(const cons_t* p)
{
  return type_of(p) == SYNTAX;
}

bool equalp(const cons_t* l, const cons_t* r)
{
  // SLOW, but sure.
  return type_of(l) != type_of(r) ? false :
          print(l) == print(r);
}

bool eqp(const cons_t* l, const cons_t* r)
{
  if ( type_of(l) != type_of(r) )
    return false;

  switch ( type_of(l) ) {
  case NIL:     return true;
  case BOOLEAN: return l->boolean == r->boolean;
  case CHAR:    return l->character == r->character;
  case INTEGER: return l->integer == r->integer;
  case DECIMAL: return l->decimal == r->decimal;
  case CLOSURE: return (l->closure->function == r->closure->function
                         && l->closure->environment == r->closure->environment);
  case PAIR:    raise(std::runtime_error("eq? is not implemented for pairs yet"));
  case SYMBOL:  return l->symbol->name() == r->symbol->name();
  case SYNTAX:  return (l->syntax->transformer == r->syntax->transformer
                         && l->syntax->environment == r->syntax->environment);
  case STRING:  return !strcmp(l->string, r->string);
  case VECTOR:  raise(std::runtime_error("Unimplemented eq? for vector")); break;
  case CONTINUATION: raise(std::runtime_error("Unimplemented eq? for continuation")); break;
  }

  return false;
}

bool eqvp(const cons_t* l, const cons_t* r)
{
  if ( type_of(l) != type_of(r) )
    return false;

  switch ( type_of(l) ) {
  case NIL:     return true;
  case BOOLEAN: return l == r;
  case SYMBOL:  return l->symbol->name() == r->symbol->name();
  case INTEGER: // Also make sure both are exact/both inexact (TODO)
                return l->integer == r->integer; 
  case DECIMAL: // Check both exact/both inexact
                return l->decimal == r->decimal;
  case CHAR:    return l->character == r->character;
  case PAIR:    return nullp(l) && nullp(r)? true : l == r;
  case VECTOR:  return l == r;
  case STRING:  return l == r;
  case SYNTAX:  return l == r; // TODO: Check against names?
  case CLOSURE: // double-check with section 6.1 and 4.1.4 (TODO)
                return l->closure == r->closure;
  case CONTINUATION:
                return l->continuation == r->continuation;
  }

  return false;
}

cons_t* append(cons_t *h, cons_t *t)
{
  return nullp(h)? t : cons(car(h), append(cdr(h), t));
}

cons_t* closure(lambda_t f, environment_t* e)
{
  closure_t *c = new closure_t();
  c->function = f;
  c->environment = e;

  cons_t *p = new cons_t();
  p->type = CLOSURE;
  p->closure = c;

  return p;
}

size_t length(const cons_t *p)
{
  size_t n = 0;

  while ( !nullp(p) ) {
    p = cdr(p);
    ++n;
  }

  return n;
}

bool not_p(const cons_t* p)
{
  // all other types + values are considered true in scheme
  return booleanp(car(p)) && car(p)->boolean == false;
}

bool and_p(const cons_t* p)
{
  // implement in terms of not_p
  return !not_p(p) && !not_p(cdr(p));
}

bool or_p(const cons_t* p)
{
  // implement in terms of not_p
  return !not_p(p) || !not_p(cdr(p));
}

bool xor_p(const cons_t* p)
{
  return !not_p(p) ^ !not_p(cdr(p));
}

double number_to_double(const cons_t* p)
{
  assert_number(p);

  switch ( type_of(p) ) {
  default:
    raise(std::runtime_error("Unsupported number->double conversion: " + sprint(p)));
  case INTEGER: return static_cast<double>(p->integer);
  case DECIMAL: return static_cast<double>(p->decimal);
  }
}

decimal_t number_to_float(const cons_t* p)
{
  return static_cast<decimal_t>(number_to_double(p));
}

bool iswhole(decimal_t n)
{
  if ( !isfinite(n) || isnan(n) || !isnormal(n) )
    return false;

  // Return true if `n` has no decimals, i.e. is "x.0" for a value of x
  // NOTE: Can possible do `(int)n == n` as well, but better to use floor.
  return (floor(n) == n) && !(n <= INT_MIN || n >= INT_MAX);
}

int gcd(int a, int b)
{
  if ( a == 0 )
    return b;

  while ( b ) {
    if ( a > b )
      a -= b;
    else
      b -= a;
  }

  return a;
}

int lcm(int a, int b)
{
  return a*b / gcd(a, b);
}

cons_t* nil_coalesce(cons_t* p)
{
  // Always return list; empty list if null
  return !nullp(p)? p : list(NULL);
}
