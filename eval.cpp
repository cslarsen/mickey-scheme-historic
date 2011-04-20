#include <stdexcept>
#include "eval.h"
#include "util.h"
#include "primops.h"
#include "primitives.h"
#include "apply.h"

cons_t* eval(cons_t* p)
{
  if ( pairp(p) ) {
    /*
     * TODO: Could I do `apply(fun, eval(cdar(p)))` below
     *       instead of calling eval() inside apply?
     */
    if ( symbolp(car(p)) )
      return apply(lookup_lambda(car(p)->symbol), cdr(p)); // apply calls eval()
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
