#include <stdexcept>
#include "eval.h"
#include "util.h"
#include "primops.h"
#include "primitives.h"
#include "apply.h"
#include "print.h"

/*
 * Magic variables to hold lambda arguments
 * and code body.  Quite the hack, and should
 * be fixed later on.  Bad because they shouldn't
 * shadow any other definitions with these names.
 */
static const char ARGS[] = "__args__";
static const char BODY[] = "__body__";

cons_t* make_closure(cons_t* args, cons_t* body, environment_t* e);

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

static cons_t* make_curried_function(cons_t *names, cons_t *values, cons_t *body, environment_t *e)
{
    // While we have (name value) pairs, set `name` => `value`
    cons_t *n = names,
           *v = values;

    while ( !nullp(n) && !nullp(v) ) {
      //printf("Currying '%s' => '%s'\n", sprint(car(n)).c_str(), sprint(car(v)).c_str());
      e->define(car(n)->symbol->name, car(v));
      n = cdr(n);
      v = cdr(v);
    }

    //printf("Remaining argument names: '%s'\n", sprint(n).c_str());
    //printf("Function Body: '%s'\n", sprint(body).c_str());

    // The rest of the parameters names, if any, is in `n`:
    // TODO: Should we set arguments in parent environment and extend in call below?
    return make_closure(n, body, e);
}

static cons_t* call_lambda(cons_t *p, environment_t* e)
{
  cons_t *args = e->symbols[ARGS];
  cons_t *body = e->symbols[BODY];

  /*
   * We have to extend the environment
   * to set up a new stack frame.
   *
   * If we DON'T do this, then we cannot
   * perform recursion, as function
   * params will never be updated (e.g.,
   * the recursive Fibonacci algorithm
   * will never terminate).
   *
   */
  e = e->extend();

  size_t params_reqd = length(args);
  size_t params_recv = length(p);

  if ( params_recv < params_reqd ) {
    // try currying (TODO: Do we need to check for any conditions?)
    return make_curried_function(args, p,
                                 body,
                                 e->extend());
  }

  if ( params_recv > params_reqd )
    throw std::runtime_error(format("Function only accepts %d parameters, but got %d", params_reqd, params_recv));

  // set up function arguments
  for ( cons_t *value = p, *name = args;
        !nullp(value) && !nullp(name);
        value = cdr(value),
        name = cdr(name) )
  {
    if ( !symbolp(car(name)) )
      throw std::runtime_error("lambda argument not a symbol but type "
        + to_s(type_of(car(name))) + ": " + sprint(car(name)));

    e->define(car(name)->symbol->name, car(value));
  }

  // now EXECUTE body with given definition
  return eval(body, e);
}

cons_t* make_closure(cons_t* args, cons_t* body, environment_t* e)
{
  // this is a hack -> will shadow params with these magic names
  e->define(ARGS, args);
  e->define(BODY, body);

  closure_t *c = new closure_t();
  c->function = call_lambda;
  c->environment = e;

  cons_t* r = new cons_t();
  r->type = CLOSURE;
  r->closure = c;
  return r;
}

// Queinnec, p. 9
static cons_t* eprogn(cons_t* exps, environment_t* env)
{
  if ( pairp(exps) )
    if ( pairp(cdr(exps)) ) {
      eval(car(exps), env);
      eprogn(cdr(exps), env);
    } else
      eval(car(exps), env);

  return nil();
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

    if ( numberp(p) || stringp(p) || charp(p) ||
         booleanp(p) || vectorp(p) || decimalp(p) ||
         closurep(p) )
    {
      return p;
    }

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

    if ( name == "set!" ) {
      cons_t *def_name = cadr(p);
      cons_t *def_body = cddr(p);

      std::string name = def_name->symbol->name;
      environment_t *i = e;

      // search for definition and set if found
      for ( ; i != NULL; i = i->outer )
        if ( i->symbols.find(name) != i->symbols.end() ) {
          i->symbols[name] = car(evlis(def_body, e));
          return nil();
        }

       // only set if NOT found
      if ( i == NULL )
        return defun_define(cons(def_name, evlis(def_body, e)), e);

      return nil();
    }

    if ( name == "lambda" ) {
      cons_t *args = cadr(p);
      cons_t *body = cddr(p);
      return make_closure(args, body, e->extend());
    }

    if ( name == "begin" )
      return eprogn(cdr(p), e);

    if ( name == "eval" )
      return eval(evlis(cdr(p), e), e);

    if ( name == "apply" ) {
      // correct to use eval on parameter list?
      return invoke( eval(cadr(p), e), eval(caddr(p), e));
    }
  }

  // skip `begin`-form; we've got that covered elsewhere (or?)
  // skip `set!` for now; we can implement it in primitives.cpp
  // skip `lambda` for now

  return invoke(  eval(car(p), e),
                 evlis(cdr(p), e));
}
