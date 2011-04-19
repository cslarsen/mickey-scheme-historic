#include "eval.h"
#include "primops.h"
#include "primitives.h"
#include "print.h"

cons_t* defun_print(cons_t *arg)
{
  for ( cons_t *p = cdr(arg); p != NULL; p = cdr(p) ) {
    if ( stringp(p) )
      printf("%s", p->string);
    else if ( integerp(p) )
      printf("%d", p->integer);
    else if ( pairp(p) ) {
      defun_print(car(p));
      defun_print(cdr(p));
    }
  }

  return arg;
}

cons_t* defun_strcat(cons_t *arg)
{
  std::string s;

  while ( arg != NULL ) {
    if ( arg->type == STRING )
      s += format("%s", arg->string);

    if ( arg->type == INTEGER )
      s += format("%d", arg->integer);

    if ( arg->type == PAIR ) {
      cons_t *res = defun_print(eval(car(arg)));
      if ( res->type == STRING )
        s += res->string; // else, throw (TODO)
    }
    
    arg = cdr(arg);
  }

  return string(s.c_str());
}

cons_t* defun_add(cons_t *arg)
{
  int sum = 0;

  while ( arg != NULL ) {
     if ( arg->type == STRING )
      continue; // TODO: throw error, or something

    if ( arg->type == INTEGER )
      sum += arg->integer;

    if ( arg->type == PAIR ) {
      cons_t *res = defun_print(eval(car(arg)));
      if ( res->type == INTEGER )
        sum += res->integer; // or else, thow (TOWO)
    }
    
    arg = cdr(arg);
  }

  return integer(sum);
}

cons_t* defun_mul(cons_t *arg)
{
  int product = 1;

  while ( arg != NULL ) {
     if ( arg->type == STRING )
      continue; // TODO: throw error, or something

    if ( arg->type == INTEGER )
      product *= arg->integer;

    if ( arg->type == PAIR ) {
      cons_t *res = defun_print(eval(car(arg)));
      if ( res->type == INTEGER )
        product *= res->integer; // else, throw (TODO)
    }
    
    arg = cdr(arg);
  }

  return integer(product);
}
