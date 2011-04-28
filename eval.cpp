#include <stdexcept>
#include "eval.h"
#include "util.h"
#include "primops.h"
#include "primitives.h"
#include "apply.h"
#include "print.h"

cons_t* eval(program_t *p)
{
  return eval(p->root, p->globals);
}

static bool bool_true(cons_t* p)
{
  return booleanp(p)? p->boolean : false;
}

static cons_t* evlis(cons_t* p, environment_t* e)
{
  return pairp(p) ?
    cons( eval(car(p), e),
          evlis(cdr(p), e)) :
    nil();
}

static cons_t* caddr(cons_t* p)
{
  return car(cddr(p));
}

static cons_t* cadddr(cons_t* p)
{
  return car(cddr(cdr(p)));
}

static cons_t* invoke(cons_t* fun, cons_t* args)
{
  if ( !closurep(fun) )
    return fun;

  environment_t *env = fun->closure->environment;
  lambda_t lambda    = fun->closure->function;

  return lambda(args, env);
}

/*
 * Based on Queinnec's evaluator numero uno.
 *
 */
cons_t* eval(cons_t* p, environment_t* e)
{
  if ( atomp(p) ) {
    if ( symbolp(p) )
      return e->lookup_or_throw(p->symbol->name);

    if ( numberp(p) || stringp(p) || charp(p) || booleanp(p) || vectorp(p) )
      return p;

    if ( closurep(p) )
      return p;

    throw std::runtime_error("Cannot evaluate: " + sprint(p));
  }


  if ( symbolp(car(p)) ) {
    std::string name = car(p)->symbol->name;

    if ( name == "quote" )
      return cadr(p);

    if ( name == "if" ) {
      if ( bool_true(eval(cadr(p), e)) )
        return eval(caddr(p), e);
      else
        return eval(cadddr(p), e);
    }

    /*
     * Define requires one not to look up
     * the variable name, so we need to take
     * special care.
     */
    if ( name == "define" ) {
      cons_t *def_name = cadr(p);
      cons_t *def_body = cddr(p);

      return defun_define(
              cons(def_name, def_body), e);
    }
  }

  // skip `begin`-form; we've got that covered elsewhere (or?)
  // skip `set!` for now; we can implement it in primitives.cpp
  // skip `lambda` for now

  return invoke(  eval(car(p), e),
                 evlis(cdr(p), e));
}
