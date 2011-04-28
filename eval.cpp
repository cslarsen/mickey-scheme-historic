#include <stdexcept>
#include "eval.h"
#include "util.h"
#include "primops.h"
#include "primitives.h"
#include "apply.h"
#include "print.h"

cons_t* eval(cons_t* p, environment_t* env)
{
  if ( pairp(p) ) {
    /*
     * TODO: Could I do `apply(fun, eval(cdar(p)))` below
     *       instead of calling eval() inside apply?
     */
    if ( closurep(car(p)) ) {
      closure_t *c = car(p)->closure;
      return apply(c->function, cdr(p), c->environment); // apply calls eval()
    } else
      return cons(eval(car(p), env), eval(cdr(p), env));
  }

  return p;
}

cons_t* eval(program_t *p)
{
  return eval(p->root, p->globals);
}
