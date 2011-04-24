#include <stdexcept>
#include "eval.h"
#include "util.h"
#include "primops.h"
#include "primitives.h"
#include "apply.h"
#include "print.h"

static cons_t* lookup_var(cons_t* var, environment_t* env)
{
  return !symbolp(var) ? var :
    env->lookup(var->symbol->name.c_str());
}

cons_t* eval(cons_t* p, environment_t* env)
{
  if ( pairp(p) ) {
    /*
     * TODO: Could I do `apply(fun, eval(cdar(p)))` below
     *       instead of calling eval() inside apply?
     */
    if ( symbolp(car(p)) ) {
      std::string name = car(p)->symbol->name;
      cons_t *cl = env->lookup(name.c_str());

      if ( nullp(cl) )
        throw std::runtime_error(format("Unknown definition '%s'", name.c_str()));

      if ( !closurep(cl) )
        throw std::runtime_error(format("Not a closure, '%s', but type *%s*: ",
          name.c_str(),
          to_s(type_of(cl)).c_str(),
          sprint(cl).c_str()));

      return apply(cl->closure->function, cdr(p), cl->closure->environment); // apply calls eval()
    } else
      return cons(eval(car(p), env), eval(cdr(p), env));
  }

  // Perform variable lookup, i.e. translate symbol "foo" to e.g. "123"
  if ( symbolp(p) ) {
    cons_t *value = lookup_var(p, env);

    if ( nullp(value) )
      throw std::runtime_error(format("Unknown variable definition '%s'", p->symbol->name.c_str()));

    return value;
  }

  // of type integer, string, etc
  return p;
}

cons_t* eval(program_t *p)
{
  return cons(eval(car(p->root), p->globals), eval(cdr(p->root), p->globals));
}
