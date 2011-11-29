/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the modified BSD license.            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include <stdexcept>
#include "eval.h"
#include "util.h"
#include "primops.h"
#include "apply.h"
#include "print.h"
#include "backtrace.h"
#include "module_base.h"
#include "exceptions.h"

/*
 * Magic variables to hold lambda arguments
 * and code body.  Quite the hack, and should
 * be fixed later on.  Bad because they shouldn't
 * shadow any other definitions with these names.
 */
static const char ARGS[] = "__args__";
static const char BODY[] = "__body__";

cons_t* make_closure(cons_t* args, cons_t* body, environment_t* e);
extern cons_t* proc_do(cons_t*, environment_t*);

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
  if ( !closurep(fun) ) {
    printf("not a closure...maybe sumthin else?\n"); // TODO
    return fun;
  }

  environment_t *env = fun->closure->environment;
  lambda_t lambda    = fun->closure->function;

  return lambda(args, env);
}

/*
static cons_t* make_curried_function(cons_t *names, cons_t *values, cons_t *body, environment_t *e)
{
    // While we have (name value) pairs, set `name` => `value`
    cons_t *n = names,
           *v = values;

    // Curry; set <name> => <value>
    while ( !nullp(n) ) {
      e->define(car(n)->symbol->name(), car(v));
      n = cdr(n);
      v = cdr(v);
    }

    // The rest of any parameters is in `n`.
    return make_closure(n, body, e);
}
*/

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
    raise(std::runtime_error("Invalid lambda signature: " + sprint(args)));

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
    raise(std::runtime_error(format("Function requires %d parameters, but got %d",
      params_reqd, params_recv)));

    // try currying (TODO: Do we need to check for any conditions?)
    //return make_curried_function(args, p, body, e->extend());
  }

  if ( params_recv > params_reqd && !has_rest ) {
    raise(std::runtime_error(format("Function only accepts %d parameters, "
                                    "but got %d", params_reqd, params_recv)));
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

        /*
         * If noe value has been set, give it a default something.
         */
        if ( nullp(value) )
          value = list(NULL);

        e->define(name->symbol->name(), value);
        break;
      } else 
        raise(std::runtime_error("Lambda argument not a symbol but type "
          + to_s(type_of(car(name))) + ": " + sprint(car(name))));
    }

    /*
     * Non-pure variadic, i.e. has some set arguments and one
     * trailing "rest"-argument.  E.g.:  (lambda (x y z . rest) <body>)
     */
    if ( car(name)->symbol->name() == "." ) {

      if ( nullp(value) ) {
        /*
         * We have a function with named argument and a rest
         * argument that has not been explicitly set.  Give it a default
         * value to avoid having it crash functions that require this
         * variable to have been bound to something.
         */
        value = list(NULL);
      }

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

cons_t* make_syntax(cons_t* body, environment_t* e)
{
  syntax_t *s = new syntax_t();
  s->transformer = body;
  s->environment = e;

  cons_t *r = new cons_t();
  r->type = SYNTAX;
  r->syntax = s;
  return r;
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

/*
 * See R7RS 4.3.2 Pattern Language for examples on how to match
 * a syntax-rules pattern with code.
 *
 * NOTE: We do NOT support the full pattern language yet.  We only
 *       support simple patterns like (<pattern>) and (<pattern> ...)
 *       and the same for templates.
 *
 *       There is also a small problem with the environments.  We should
 *       capture free variables from the environment where the macro was
 *       DEFINED, and reuse that... Currently, we don't do that.
 *
 */
static bool syntax_match(cons_t *pat, cons_t *c, dict_t& map)
{
  for ( ; !nullp(pat); pat = cdr(pat) ) {
    cons_t *l = car(pat);
    cons_t *r = car(c);

    // Match REST of symbols (TODO: Fix this, it's not very correct)
    if ( l->symbol->name() == "..." ) {
      map["..."] = c;
      return true;
    }

    // Pattern is too long; no match
    if ( nullp(c) )
      return false;

    map[l->symbol->name()] = r;
    c = cdr(c);
  }

  // Check that pattern was fully matched
  return nullp(pat);
}

static cons_t* deep_copy(const cons_t *p)
{
  if ( nullp(p) )
    return nil();

  cons_t *r = new cons_t();
  memcpy(r, p, sizeof(cons_t));

  if ( listp(r) ) {
    r->car = deep_copy(r->car);
    r->cdr = deep_copy(r->cdr);
  } else if ( syntaxp(r) ) {
    r->syntax->transformer = deep_copy(r->syntax->transformer);
  } else if ( stringp(r) ) {
    const char *old = r->string;
    r->string = (const char*) malloc(strlen(old));
    strcpy(const_cast<char*>(r->string), old);
  }

  return r;
}

static cons_t* syntax_replace(dict_t &map, cons_t* p)
{
  cons_t *start = p;

  for ( ; !nullp(p); p = cdr(p) ) {
    if ( listp(car(p)) )
      p->car = syntax_replace(map, car(p));
    else if ( symbolp(car(p)) && map.count(car(p)->symbol->name()) )
      p->car = map[car(p)->symbol->name()];
  }

  return start;
}

static cons_t* syntax_expand(cons_t *macro, cons_t *code, environment_t*)
{
  //cons_t *name = car(code);
  cons_t *rules = macro->syntax->transformer;
  //environment_t* macro_env = macro->syntax->environment;

  //printf("  --> got name   = '%s'\n", sprint(name).c_str());
  //printf("  --> got rules  = '%s'\n", sprint(car(rules)).c_str());
  //printf("  --> got code   = '%s'\n", sprint(code).c_str());

  // Go through all rules and find a match
  for ( cons_t *p = cdar(rules); !nullp(p); p = cdr(p) ) {
    cons_t *pattern = caar(p);
    cons_t *expansion = cadar(p);

    //printf("comparing rule:\n");
    //printf("  pattern: '%s'\n", sprint(pattern).c_str());
    //printf("  code: '%s'\n", sprint(code).c_str());
    //printf("  expansion: '%s'\n", sprint(expansion).c_str());

    dict_t map;

    if ( syntax_match(pattern, code, map) )
      return syntax_replace(map, deep_copy(expansion));
  }

  // What if there's no match?

  return code;
}

static cons_t* invoke_with_trace(cons_t* op, cons_t* args, environment_t* e)
{
  backtrace_push(cons(op, args));

  //printf("invoke op='%s' args='%s'\n", sprint(op).c_str(), sprint(args).c_str());

  cons_t *fun = eval(op, e);
  cons_t *r = syntaxp(fun)?
    eval(syntax_expand(fun, cons(op, args), e), e) :
    invoke(fun, evlis(args, e));

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

static cons_t* eval_quasiquote(cons_t* p, environment_t* e)
{
  // Find unquote
  if ( symbolp(car(p)) ) {
    std::string n = car(p)->symbol->name();

    // TODO: Only allow this if we're explicitly inside quasiquote
    if ( n == "unquote" || n == "," )
      return eval(cdr(p), e);
  }

  return !pairp(p) ? p :
    cons(eval_quasiquote(car(p), e),
         eval_quasiquote(cdr(p), e));
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
         closurep(p) || syntaxp(p) )
    {
      return p;
    }

    raise(std::runtime_error("Cannot evaluate: " + sprint(p)));
  }

  if ( symbolp(car(p)) ) {
    std::string name = car(p)->symbol->name();

    if ( name == "quote" )
      return cadr(p);

    if ( name == "quasiquote" )
      return eval_quasiquote(cadr(p), e);

    if ( name == "if" ) {
      /*
       * Handle both (if <test> <true-action> <false-action>) [1]
       * and (if <test> <true-action>)                        [2]
       */
      if ( bool_true(eval(cadr(p), e)) ) // cases [1, 2]
        return eval(caddr(p), e);
      else if ( !nullp(cadddr(p)) )      // case [1]
        return eval(cadddr(p), e);
      else
        return nil();                    // case [2]
    }

    if ( name == "cond" ) {
      cons_t *cond = proc_cond(p, e);
      return eval(cond, e);
    }

    if ( name == "define-syntax" ) {
      cons_t *name = cadr(p);
      cons_t *body = cddr(p);
      cons_t *syntax = make_syntax(body, e->extend());
      return proc_define_syntax(cons(name, cons(syntax)), e);
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
        return proc_define(cons(def_name, cons(closure)), e);
      }

      /*
       * Ordinary `(define <name> <body>)`, where <body> can typically
       * be `(lambda (<arg1 arg2 ...>) <body>)`.
       */
      return proc_define(
              cons(def_name, evlis(def_body, e)), e);
    }

    if ( name == "set!" ) {
      cons_t *def_name = cadr(p);
      cons_t *def_body = cddr(p);

      std::string name = def_name->symbol->name();
      environment_t *i = e;

      // search for definition and set if found
      for ( ; i != NULL; i = i->outer ) {
        if ( i->symbols.find(name) != i->symbols.end() ) {
          i->symbols[name] = car(evlis(def_body, e));
          return nil();
        }
      }

       // only set if NOT found
      if ( i == NULL )
        return proc_define(cons(def_name, evlis(def_body, e)), e);

      return nil();
    }

    if ( name == "set-car!" )
      return proc_set_car(cons(cadr(p), evlis(cddr(p), e)), e);

    if ( name == "set-cdr!" )
      return proc_set_cdr(cons(cadr(p), evlis(cddr(p), e)), e);

    if ( name == "lambda" ) {
      cons_t *args = cadr(p);
      cons_t *body = cddr(p);

      // capture no argument lambda `(lambda () do-something)`
      if ( nullp(body) && !nullp(args) )
      {
        // We have a `(lambda () <body>)` form
        args = list(NULL);
        body = cons(cadr(p));
      }

      return make_closure(args, body, e->extend());
    }

    if ( name == "begin" )
      return eprogn(cdr(p), e);

    if ( name == "let" )
      return eval(proc_let(cdr(p), e), e);

    if ( name == "let*" )
      return eval(proc_letstar(cdr(p), e), e);

    if ( name == "letrec" )
      return eval(proc_letrec(cdr(p), e), e);

    if ( name == "do" )
      return eval(proc_do(p, e), e);

    if ( name == "eval" )
      return eval_with_trace(cdr(p), e);

    if ( name == "apply" ) {
      // Correct to use eval instead of evlis (or nothing) on parameter list?
      return invoke_with_trace(cadr(p), eval(caddr(p), e), e);
    }

    if ( name == "delay" ) {
      /*
       * Convert to (lambda () <body>)
       *
       * TODO: Lazy evaluation should MEMOIZE, per the
       *       standard.  Add this. (OR, make a macro-system,
       *       and implement delay via that.)
       */
      return eval(cons(symbol("lambda", e),
                    cons(list(NULL),
                      cons(cadr(p)))), e);
    }

    if ( name == "force" )
      return eval(list(cadr(p)), e);
  }

  // skip `begin`-form; we've got that covered elsewhere (or?)
  // skip `set!` for now; we can implement it in primitives.cpp
  // skip `lambda` for now
  return invoke_with_trace(car(p), cdr(p), e);
}
