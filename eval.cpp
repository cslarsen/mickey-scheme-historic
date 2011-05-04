#include <stdexcept>
#include "eval.h"
#include "util.h"
#include "primops.h"
#include "primitives.h"
#include "apply.h"
#include "print.h"
#include "backtrace.h"


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

    // Curry; set <name> => <value>
    while ( !nullp(n) && !nullp(v) ) {
      e->define(car(n)->symbol->name(), car(v));
      n = cdr(n);
      v = cdr(v);
    }

    // The rest of any parameters is in `n`.
    return make_closure(n, body, e);
}

/*
 * Return number of required arguments, excluding
 * "rest" arguments, e.g. "(x y z . rest)" or "rest".
 */
static size_t arg_length(cons_t* args)
{ 
  size_t count  = 0;
  bool prev_dot = false;

  cons_t *p = args;

  // pure variadic function
  if ( !nullp(args) && length(args)==1 && !symbolp(car(args)) )
    return 0;

  for ( ; !nullp(p); p = cdr(p) ) {
    if ( symbolp(car(p)) && car(p)->symbol->name() == "." )
      prev_dot = true;
    else {
      if ( prev_dot ) {
        p = cdr(p); // so as not to break test below
        break;
      }
      ++count;
    }
  }

  if ( length(p) != 0 )
    throw std::runtime_error("Invalid lambda signature: " + sprint(args));

  return count;
}

static bool has_rest_args(cons_t* p)
{
  return !listp(p) || (arg_length(p) < length(p));
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

  bool   has_rest    = has_rest_args(args);
  size_t params_reqd = arg_length(args);
  size_t params_recv = length(p);

  if ( params_recv < params_reqd ) {
    // try currying (TODO: Do we need to check for any conditions?)
    return make_curried_function(args, p, body, e->extend());
  }

  if ( params_recv > params_reqd && !has_rest ) {
    throw std::runtime_error(format("Function only accepts %d parameters, "
                                    "but got %d", params_reqd, params_recv));
  }

  // Set up function arguments
  cons_t *value = p, *name = args;
  for ( ; !nullp(name);
        value = cdr(value), name = cdr(name) )
  {
    if ( !symbolp(car(name)) ) {
      if ( has_rest ) {
        // Pure variadic function, e.g. (lambda x (display x)) will have
        // all args in `x`.
        e->define(name->symbol->name(), value);
        break;
      } else 
        throw std::runtime_error("Lambda argument not a symbol but type "
          + to_s(type_of(car(name))) + ": " + sprint(car(name)));
    }

    /*
     * Non-pure variadic, i.e. has some set arguments and one
     * trailing "rest"-argument.  E.g.:  (lambda (x y z . rest) <body>)
     */
    if ( car(name)->symbol->name() == "." ) {
      e->define(cadr(name)->symbol->name(), value);
      break;
    }

    if ( nullp(value) )
      break;

    e->define(car(name)->symbol->name(), car(value));
  }

  // now EXECUTE body with given definition
  return eval(body, e);
}

cons_t* make_closure(cons_t* args, cons_t* body, environment_t* e)
{
  // Wrap body in begin-block (or else our poor evaluator will execute
  // expressions backwards!)... this is of course an indication that
  // something else is terribly wrong! :-)
  body = cons(symbol("begin", e), body);

  // This is a hack -> will shadow params with these magic names (FIXME)
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

/*
 * Based on Queinnec, p. 9
 *
 * A problem, though, is that we wrap lots of code inside
 * (begin <body>) blocks, which is handled here.  The problem
 * is that for code like (begin (< 1 2)) it's a bit difficult
 * to reason about where in the code to return.
 *
 * So, I've tried my best below. (TODO: Make LOTS of tests for this!)
 *
 */
static cons_t* eprogn(cons_t* exps, environment_t* env)
{
  if ( pairp(exps) )
    if ( pairp(cdr(exps)) ) {
      eval(car(exps), env);
      return eprogn(cdr(exps), env); // originally had no return
    } else
      return eval(car(exps), env); // originally had no return

  return nil();
}

static cons_t* invoke_with_trace(cons_t* op, cons_t* args, environment_t* e)
{
  backtrace_push(cons(op, args));
  cons_t *r = invoke(eval(op, e), evlis(args, e));
  backtrace_pop();
  return r;
}

static cons_t* eval_with_trace(cons_t* expr, environment_t* e)
{
  backtrace_push(expr);
  cons_t *result = eval( evlis(expr, e), e);
  backtrace_pop();
  return result;
}

/*
 * Based on Queinnec's evaluator numero uno.
 *
 */
cons_t* eval(cons_t* p, environment_t* e)
{
  if ( atomp(p) ) {
    if ( symbolp(p) )
      return e->lookup_or_throw(p->symbol->name());

    if ( numberp(p) || stringp(p) || charp(p) ||
         booleanp(p) || vectorp(p) || decimalp(p) ||
         closurep(p) )
    {
      return p;
    }

    throw std::runtime_error("Cannot evaluate: " + sprint(p));
  }

  if ( symbolp(car(p)) ) {
    std::string name = car(p)->symbol->name();

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

      /*
       * Handle implicit lambda/short form, that is, a
       * `(define (foo <arg1 arg2 ...>) <body>)`-style
       * definition.
       */
      if ( listp(def_name) ) {
        cons_t *def_args = cdr(def_name);
        def_name = car(def_name);

        cons_t *closure = make_closure(def_args, def_body, e->extend());
        return defun_define(cons(def_name, cons(closure)), e);
      }

      /*
       * Ordinary `(define <name> <body>)`, where <body> can typically
       * be `(lambda (<arg1 arg2 ...>) <body>)`.
       */
      return defun_define(
              cons(def_name, evlis(def_body, e)), e);
    }

    if ( name == "set!" ) {
      cons_t *def_name = cadr(p);
      cons_t *def_body = cddr(p);

      std::string name = def_name->symbol->name();
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

      if ( symbolp(car(p)) && car(p)->symbol->name() == "lambda"
            && length(p) == 2 && listp(cadr(p)) )
      {
        /*
         * We have a `(lambda () <body>)` form
         */
        args = list(NULL);
        body = cons(cadr(p));
      }

      return make_closure(args, body, e->extend());
    }

    if ( name == "begin" )
      return eprogn(cdr(p), e);

    if ( name == "eval" )
      return eval_with_trace(evlis(cdr(p), e), e);

    if ( name == "apply" ) {
      // correct to use eval on parameter list?
      return invoke_with_trace(cadr(p), caddr(p), e);
    }
  }

  // skip `begin`-form; we've got that covered elsewhere (or?)
  // skip `set!` for now; we can implement it in primitives.cpp
  // skip `lambda` for now

  return invoke_with_trace(car(p), cdr(p), e);
}
