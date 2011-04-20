#include <stdexcept>
#include "eval.h"
#include "util.h"
#include "primops.h"
#include "primitives.h"
#include "apply.h"

lambda_t lookup_lambda(symbol_t *s)
{
  // TODO: Put into table
  std::string n = !s? "" : toupper(s->name.c_str());

  if ( n == "DISPLAY" ) return defun_print;
  if ( n == "STRING-APPEND" ) return defun_strcat;
  if ( n == "+" ) return defun_add;
  if ( n == "*" ) return defun_mul;

  return NULL;
}

cons_t* eval(cons_t* p)
{
  if ( pairp(p) ) {
    /*
     * TODO: Could I do `apply(fun, eval(cdar(p)))` below
     *       instead of calling eval() inside apply?
     */
    if ( symbolp(car(p)) )
      return apply(lookup_lambda(car(p)->symbol), cdr(p)); // apply calls eval()A
    else
      return cons(eval(car(p)), eval(cdr(p)));
  }

  // other data types simply evaluate to themselves
  // TODO: look up variable values here

  return p;
}

cons_t* eval(program_t *p)
{
  return cons(eval(car(p->root)), eval(cdr(p->root)));
}
