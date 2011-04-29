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
  return !pairp(p) ? nil() :
    cons(eval(car(p), e),
         evlis(cdr(p), e));
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

static size_t length(cons_t *p)
{
  size_t n = 0;

  while ( !nullp(p) ) {
    ++n; p = cdr(p);
  }

  return n;
}

static cons_t* call_lambda(cons_t *p, environment_t* e)
{
  size_t params_reqd = length(e->symbols["args"]);
  size_t params_recv = length(p);

  if ( params_recv < params_reqd )
    throw std::runtime_error(format("Not enough arguments to function, need %d but only got %d", params_reqd, params_recv));

  if ( params_recv > params_reqd )
    throw std::runtime_error(format("Function only accepts %d parameters, but got %d", params_reqd, params_recv));

  // set up function arguments
  for ( cons_t *value = p, *name = e->symbols["args"];
        !nullp(value) && !nullp(name);
        value = cdr(value),
        name = cdr(name) )
  {
    if ( !symbolp(car(name)) )
      throw std::runtime_error("lambda argument not a symbol but type "
        + to_s(type_of(car(name))) + ": " + sprint(car(name)));

//    printf("setting '%s' = '%s'\n", car(name)->symbol->name.c_str(), sprint(car(value)).c_str());
    e->define(car(name)->symbol->name, car(value));
  }

  cons_t *body = e->symbols["body"];
//  printf("body is '%s'\n", sprint(body).c_str());
  // now EXECUTE body with given definition
  return eval(body, e);
}

cons_t* make_closure(cons_t* args, cons_t* body, environment_t* e)
{
//  printf("make closure with args '%s'\n", sprint(args).c_str());
//  printf("make closure with body '%s'\n", sprint(body).c_str());

  e->define("args", args);
  e->define("body", body);

  closure_t *c = new closure_t();
  c->function = call_lambda;
  c->environment = e;

  cons_t* r = new cons_t();
  r->type = CLOSURE;
  r->closure = c;
  return r;
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

      // TODO: If def_name is a list, we've
      // got an implicit lambda def:
      // "(define (name arg1 arg2 ...) (* body of func))"

      return defun_define(
              cons(def_name, evlis(def_body, e)), e);
    }

    if ( name == "lambda" ) {
      cons_t *args = cadr(p);
      cons_t *body = cddr(p);
      return make_closure(args, body, e->extend());
    }
  }

  // skip `begin`-form; we've got that covered elsewhere (or?)
  // skip `set!` for now; we can implement it in primitives.cpp
  // skip `lambda` for now

  return invoke(  eval(car(p), e),
                 evlis(cdr(p), e));
}
