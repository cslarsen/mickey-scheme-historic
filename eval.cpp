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
  cons_t *r = NULL;

  while ( !nullp(p) ) {
    if ( !listp(p) )
      r = append(r, eval(p, e));
    else
      r = append(r, eval(car(p), e)); // need this?

    p = cdr(p);
  }

  return r;
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
    throw std::runtime_error("Not a closure: " + sprint(fun));

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
      return e->lookup(p->symbol->name);

    if ( numberp(p) || stringp(p) || charp(p) || booleanp(p) || vectorp(p) )
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

    // skip `begin`-form; we've got that covered elsewhere (or?)
    // skip `set!` for now; we can implement it in primitives.cpp
    // skip `lambda` for now

    return invoke( eval(car(p), e),
                   evlis(cdr(p), e));
  }

  // what now?

  return p;
}
