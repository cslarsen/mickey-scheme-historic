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
    if ( stringp(arg) )
      s += format("%s", arg->string);

    if ( integerp(arg) )
      s += format("%d", arg->integer);

    if ( pairp(arg) ) {
      cons_t *res = defun_print(eval(car(arg)));

      if ( stringp(res) )
        s += res->string; // else, throw (TODO)
    }
    
    arg = cdr(arg);
  }

  return string(s.c_str());
}

cons_t* defun_add(cons_t *p)
{
  /*
   * Integers have an IDENTITY, so we can do this,
   * but a more correct approach would be to take
   * the value of the FIRST number we find and
   * return that.
   */
  int sum = 0;

  for ( ; p != NULL; p = cdr(p) ) {
     if ( stringp(p) )
      continue; // TODO: throw error, or something

    if ( integerp(p) )
      sum += p->integer;

    if ( pairp(p) ) {
      cons_t *res = eval(car(p));
      if ( integerp(res) )
        sum += res->integer; // or else, thow (TOWO)
    }
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
