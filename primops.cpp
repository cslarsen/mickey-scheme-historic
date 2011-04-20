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
  cons_t *p = new cons_t();
  p->type = SYMBOL;
  p->symbol = symbol_t::create_symbol(s, env);
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
  type_t t = type_of(p);
  return t==SYMBOL || t==INTEGER || t==STRING;
}

bool integerp(cons_t* p)
{
  return type_of(p) == INTEGER;
}

bool stringp(cons_t* p)
{
  return type_of(p) == STRING;
}

bool nullp(cons_t* p)
{
  return type_of(p) == NIL;
}

bool pairp(cons_t* p)
{
  return type_of(p) == PAIR;
}

cons_t* append(cons_t *h, cons_t *t)
{
  if ( h == NULL )
    return t;
  else if ( !pairp(h) )
    return NULL; // error; try running "(append 1 nil)", should throw error
  else if ( car(h) == NULL )
    h->car = t;
  else if ( cdr(h) == NULL )
    h->cdr = t;
  else
    append(cdr(h), t);

  return h;
}
