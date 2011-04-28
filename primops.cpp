#include <stdexcept>
#include "primops.h"
#include "util.h"

cons_t* cons(cons_t* head, cons_t* tail)
{
  cons_t *p = new cons_t();
  p->type = PAIR;
  p->car = head;
  p->cdr = tail;
  return p;
}

cons_t* list(cons_t* head, cons_t* tail)
{
  if ( tail == NULL )
    return cons(head, tail);
  else
    return cons(head, cons(tail, NULL));
}

cons_t* symbol(const char* s, environment_t *env)
{
  // NOTE: A variable->value lookup is performed here
  // This is probably not the correct place to do this!
  // 
  return env->create_symbol(s);
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

cons_t* boolean(bool f)
{
  cons_t *p = new cons_t();
  p->type = BOOLEAN;
  p->boolean = f;
  return p;
}

cons_t* string(const char* s)
{
  cons_t *p = new cons_t();
  p->type = STRING;
  p->string = copy_str(s);
  return p;
}

cons_t* car(cons_t* p)
{
  return ( p != NULL && p->type == PAIR ) ? p->car : NULL;
}

cons_t* cdr(cons_t* p)
{
  return ( p != NULL && p->type == PAIR ) ? p->cdr : NULL;
}

cons_t* caar(cons_t* p)
{
  return car(car(p));
}

cons_t* cadr(cons_t* p)
{
  return car(cdr(p));
}

cons_t* cddr(cons_t* p)
{
  return cdr(cdr(p));
}

cons_t* cdar(cons_t* p)
{
  return cdr(car(p));
}

type_t type_of(cons_t* p)
{
  return p == NULL ? NIL : p->type;
}

bool symbolp(cons_t* p)
{
  return type_of(p) == SYMBOL;
}

bool atomp(cons_t* p)
{
  return !pairp(p); // Queinnec, p. 4
}

bool integerp(cons_t* p)
{
  return type_of(p) == INTEGER;
}

bool booleanp(cons_t* p)
{
  return type_of(p) == BOOLEAN;
}

bool stringp(cons_t* p)
{
  return type_of(p) == STRING;
}

bool nullp(cons_t* p)
{
  return type_of(p) == NIL
    || (type_of(p)==PAIR && type_of(car(p))==NIL && type_of(cdr(p))==NIL);
}

bool pairp(cons_t* p)
{
  // (1) A pair is a list, (2) except for the empty list '()
  return listp(p) &&                   // (1)
    !(nullp(car(p)) && nullp(cdr(p))); // (2)
}

bool listp(cons_t* p)
{
  return type_of(p) == PAIR;
}

bool closurep(cons_t* p)
{
  return type_of(p) == CLOSURE;
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
