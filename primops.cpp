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

#include <math.h> // floor
#include <stdexcept>
#include "primops.h"
#include "util.h"
#include "assertions.h"
#include "print.h"

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

cons_t* symbol(const char* s, environment_t *env)
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

cons_t* decimal(float n)
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

cons_t* cdar(const cons_t* p)
{
  return cdr(car(p));
}

cons_t* cadar(const cons_t* p)
{
  return car(cdar(p));
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

bool equalp(const cons_t* l, const cons_t* r)
{
  // SLOW, but sure.
  if ( type_of(l) == type_of(r) )
    return sprint(l) == sprint(r);
  return false;
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
  case PAIR:    throw std::runtime_error("eq? is not implemented for pairs yet");
  case SYMBOL:  return l->symbol->name() == r->symbol->name();
  case STRING:  return !strcmp(l->string, r->string);
  case VECTOR:  throw std::runtime_error("Unimplemented eq? for vector"); break;
  case CONTINUATION: throw std::runtime_error("Unimplemented eq? for continuation"); break;
  }

  return false;
}

cons_t* append(cons_t *h, cons_t *t)
{
  if ( nullp(h) || !listp(h) )
    return t; //throw std::runtime_error("First argument to (append) must be a list");
  else if ( nullp(car(h)) )
    h->car = t;
  else if ( nullp(cdr(h)) )
    h->cdr = t;
  else
    append(cdr(h), t);

  return h;
}

cons_t* append_non_mutable(cons_t *h, cons_t *t)
{
  cons_t *r = new cons_t();
  memcpy(r, h, sizeof(cons_t));

  if ( nullp(r) || !listp(r) )
    return t; //throw std::runtime_error("First argument to (append) must be a list");
  else if ( nullp(car(r)) )
    r->car = t;
  else if ( nullp(cdr(r)) )
    r->cdr = t;
  else
    append(cdr(r), t);

  return r;
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
    throw std::runtime_error("Unsupported number->double conversion: " + sprint(p));
  case INTEGER: return static_cast<double>(p->integer);
  case DECIMAL: return static_cast<double>(p->decimal);
  }
}

float number_to_float(const cons_t* p)
{
  return static_cast<float>(number_to_double(p));
}

bool iswhole(float n)
{
  // Return true if `n` has no decimals, i.e. is "x.0" for a value of x
  // NOTE: Can possible do `(int)n == n` as well, but better to use floor.
  return floor(n) == n;
}
