/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include "mickey.h"

cons_t* eval(program_t *p)
{
  return eval(p->root, p->globals);
}

static bool is_self_evaluating(cons_t* p)
{
  // TODO: Implement as switch(type_of(p)) ...
  return
    numberp(p) || stringp(p) || charp(p) ||
    booleanp(p) || vectorp(p) || decimalp(p) ||
    closurep(p) || syntaxp(p) || portp(p) ||
    environmentp(p) || emptylistp(p);
}

extern "C" { // because I don't like name-mangling

/*
 * Magic variables to hold lambda arguments
 * and code body.  Quite the hack, and should
 * be fixed later on.  Bad because they shouldn't
 * shadow any other definitions with these names.
 */
const char ARGS[] = "__args__";
const char BODY[] = "__body__";

static std::string func_name;

static cons_t* make_closure(cons_t* args, cons_t* body, environment_t* e);

static cons_t* invoke(cons_t* fun, cons_t* args)
{
  if ( !closurep(fun) )
    return fun;

  environment_t *env = fun->closure->environment;
  lambda_t lambda    = fun->closure->function;

  return lambda(args, env);
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

  if ( params_recv < params_reqd )
    raise(runtime_exception(format("Function requires %d parameters, but got %d",
      params_reqd, params_recv)));

  if ( params_recv > params_reqd && !has_rest ) {
    raise(runtime_exception(
      format("Function '%s' only accepts %d parameters, "
             "but got %d", func_name.c_str(),
             params_reqd, params_recv)));
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
        raise(runtime_exception("Lambda argument not a symbol but type "
          + to_s(type_of(car(name))) + ": " + sprint(car(name))));
    }

    /*
     * Non-pure variadic, i.e. has some set arguments and one
     * trailing "rest"-argument.  E.g.:  (lambda (x y z . rest) <body>)
     *
     */
    if ( !pairp(name) ) {

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

struct body_env_t {
  cons_t *body;
  environment_t *env;
};

static body_env_t expand_lambda(cons_t *p, environment_t* e)
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

  if ( params_recv < params_reqd )
    raise(runtime_exception(format("Function requires %d parameters, but got %d",
      params_reqd, params_recv)));

  if ( params_recv > params_reqd && !has_rest ) {
    raise(runtime_exception(
      format("Function '%s' only accepts %d parameters, "
             "but got %d", func_name.c_str(),
             params_reqd, params_recv)));
  }

  // Set up function arguments
  cons_t *value = p, *name = args;
  for ( ; !nullp(name);
        value = cdr(value), name = cdr(name) )
  {
    if ( !symbolp(car(name)) ) {
      if ( has_rest ) {
        /*
         * Pure variadic function, e.g. (lambda x (display x)) will have
         * all args in `x`.
         */

        /*
         * If no value has been set, give it a default something.
         */
        if ( nullp(value) )
          value = list(NULL);

        e->define(name->symbol->name(), value);
        break;
      } else 
        raise(runtime_exception("Lambda argument not a symbol but type "
          + to_s(type_of(car(name))) + ": " + sprint(car(name))));
    }

    /*
     * Non-pure variadic, i.e. has some set arguments and one
     * trailing "rest"-argument.  E.g.:  (lambda (x y z . rest) <body>)
     */
    if ( !pairp(name) ) {
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

  body_env_t r;
  r.body = body;
  r.env = e;
  return r;
}

static cons_t* make_closure(cons_t* args, cons_t* body, environment_t* e)
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
 * Invoke function while making a stack trace.
 *
 * Added evlis_args option to disable evlis'ing arguments.
 * This is needed by e.g. (apply), which needs to EVAL its
 * arguments.
 */
static cons_t* invoke_with_trace(
  cons_t* op,
  cons_t* args,
  environment_t* env,
  bool evlis_args = true)
{
  backtrace_push(cons(op, args));
  cons_t *fun = eval(op, env); // lookup function

  if ( !closurep(fun) )
    raise(runtime_exception(format(
      "Not a function: %s", sprint(op).c_str())));

  if ( symbolp(op) )
    func_name = op->symbol->name();
  else
    func_name = "<?>";

  /*
   * If closure is syntactic, instead of a normal function,
   * we don't want to evaluate procedure arguments.
   */
  if ( fun->closure->syntactic )
    evlis_args = false;

  cons_t *ret = invoke(fun, evlis_args? evlis(args, env) : args);

  func_name = "";
  backtrace_pop();
  return ret;
}

static cons_t* eval_quasiquote(cons_t* p, environment_t* e)
{
  if ( nullp(p) )
    return p;

  cons_t *r = list();

  /*
   * We use a tail-pointer t to avoid using the slow function
   * append().
   */
  for ( cons_t *t = r; !nullp(p); p = cdr(p) ) {
    if ( !listp(car(p)) )
      t->car = car(p);
    else {
      const std::string name = symbol_name(caar(p));

      if ( name == "unquote-splicing" ) {
        t = splice_into(eval(cadar(p), e), t);
        continue;
      } else if ( name == "unquote" )
        t->car = eval(cadar(p), e);
      else
        t->car = eval_quasiquote(car(p), e);
    }

    t = t->cdr = cons(NULL);
  }

  return r;
}

/*
 * Based on Queinnec's evaluator numero uno AND
 * Norvig et al's JScheme properly tail recursive eval.
 *
 */
cons_t* eval(cons_t* p, environment_t* e)
{
  for(;;) {
    if ( atomp(p) ) {
      if ( symbolp(p) )
        return e->lookup_or_throw(p->symbol->name());

      if ( is_self_evaluating(p) )
        return p;

      std::string s = sprint(p);

      if ( s.empty() )
        s = format("#<?unknown_type? %p>", p);

      raise(runtime_exception("Cannot evaluate: " + s));
    }

    if ( symbolp(car(p)) ) {
      const std::string name = car(p)->symbol->name();

      if ( name == "quote" ) {
        assert_length(cdr(p), 1);
        return cadr(p);
      }

      if ( name == "quasiquote" ) {
        assert_length(cdr(p), 1);
        return car(eval_quasiquote(cdr(p), e));
      }

      if ( name == "if" ) {
        /*
         * Handle both (if <test> <true-action> <false-action>) [1]
         * and (if <test> <true-action>)                        [2]
         *
         * Note: Previously we did `return eval(caddr(p), e)´ here,
         *       but now we're taking cues form http://norvig.com/lispy2.html
         *
         * Also note that ANY test is considered true unless it is
         * explicitly #f.  So (if 123 'ok) returns ok.
         *
         */
        if ( !boolean_false(eval(cadr(p), e)) ) { // cases [1, 2]
          p = caddr(p);
          continue;
        } else if ( !nullp(cadddr(p)) ) {    // case [1]
          p = cadddr(p);
          continue;
        } else
          return nil();                      // case [2]
      }

      if ( name == "cond" )
        return eval(proc_cond(p, e), e);

      if ( name == "case" )
        return eval(proc_case(p, e), e);

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
        if ( pairp(def_name) ) {
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

      if ( name == "lambda" ) {
        cons_t *args = cadr(p);
        cons_t *body = cddr(p);

        // capture no argument lambda `(lambda () do-something)`
        if ( nullp(body) && !nullp(args) ) {
          // We have a `(lambda () <body>)` form
          args = list(NULL);
          body = cons(cadr(p));
        }

        return make_closure(args, body, e->extend());
      }

      /*
       * Again, we steal some ideas from Norvig's lispy2
       * http://norvig.com/lispy2.html
       */
      if ( name == "begin" ) {
        p = cdr(p);
  EPROGN:
        if ( pairp(p) ) {
          if ( pairp(cdr(p)) ) {
            eval(car(p), e);
            p = cdr(p);
            goto EPROGN;
          } else {
            p = car(p);
            continue;
          }
        }

        return nil();
      }

      if ( name == "vector" )
        return eval(proc_vector(cdr(p), e), e);

      if ( name == "let" )
        return eval(proc_let(cdr(p), e), e);

      if ( name == "let*" )
        return eval(proc_letstar(cdr(p), e), e);

      if ( name == "letrec" )
        return eval(proc_letrec(cdr(p), e), e);

      if ( name == "do" )
        return eval(proc_do(p, e), e);

      if ( name == "set-car!" )
        return proc_set_car(cdr(p), e);

      if ( name == "set-cdr!" )
        return proc_set_cdr(cdr(p), e);

      if ( name == "eval" ) {
        p = car(evlis(cdr(p), e));
        continue;
      }

      if ( name == "apply" ) {
        /*
         * Difference between apply and normal
         * function evaluation is that (1) we
         * run eval on the parameters instead of
         * evlis, and (2) that apply takes a
         * single list of procedure arguments
         * that are "spliced in", so that
         * (apply proc (list a b c ...)) is
         * effectively expanded to * (proc a b c ...)
         */

        assert_length(cddr(p), 1);
        assert_proper_list(cddr(p));

        cons_t *proc = cadr(p);
        cons_t *args = eval(caddr(p), e);
        bool use_evlis = false;

        return invoke_with_trace(proc, args, e, use_evlis);
      }
    }

    /*
     * Again, per JScheme authors Norvig et al -- who may or may not
     * have gotten this from Queinnec/SICP/AI-memos -- we'll eliminate
     * proper tail calls!
     *
     * Tactic:  For user-defined function, we'll evaluate all the
     * function arguments (using evlis) and pass the results to
     * expand_lambda.  This function will in turn take these arguments
     * and put them into the function's expected input parameters.
     * When finished, expand_lambda will return the closure's environment
     * (containing the function's input arguments with values) as well
     * as a function body (basically an expanded closure).
     *
     * Now, instead of recursively calling eval() on this, we'll just
     * pass this on as the next step to call in this eval-loop.
     *
     * In this way, for functions that are PROPERLY and EXPLICITLY
     * tail-recursive, we will not grow the call stack infinitely!
     *
     * This means that you can now call endless recursive loops like
     * the following, without crashing:  
     *
     *   (define (a) (display "Yeah") (a)) ; <- will not crash!
     *
     */
    if ( symbolp(car(p)) ) {
      cons_t *op = e->lookup_or_throw(car(p)->symbol->name());

      if ( closurep(op) ) {
        cons_t *body = op->closure->environment->symbols[BODY];

        if ( !nullp(body) ) {
          func_name = car(p)->symbol->name();
          body_env_t r = expand_lambda(evlis(cdr(p), e), op->closure->environment);
          func_name = "";
          p = r.body;
          e = r.env;
          continue;
        }
      } else if ( syntaxp(op) ) {
        /*
         * We need to expand macros in the environment
         * in which they were defined, so we cannot just
         * do
         *
         * p = syntax_expand(op, p, e);
         *
         * We can't simply eval() the expansion either
         * in the syntax environment, because then the
         * syntax would not be able to see any outside
         * definitions that we might want the macro
         * to recognize, e.g., the following wouldn't
         * work:
         *
         * (unless local-definition 'it works)
         *
         * So we are forced to do some tricks with the
         * environment.
         */
        environment_t *synenv = op->syntax->environment;

        if ( synenv->outer == NULL )
          synenv->outer = e;
        else
          merge(synenv->outer, e);

        return eval(
            syntax_expand(op, p, op->syntax->environment),
            synenv);

        /*
         * Some background info on the above:
         *
         * Previously, this did not work in mickey:
         *
         * (import
         *   (prefix (scheme base) b-) ; unless is now b-unless
         *   (scheme write)) ; we need display
         *
         * (define dont-wanna-print #f) ; local definition
         * (b-unless dont-wanna-print (display "It works!\n"))
         * ; b-unless expands to (if (not <expr>) <actions>),
         * ; so must now be able to find both the 'not' definition
         * ; in scheme base, and the dont-wanna-print definition
         * ; in the current environment.
         *
         * Above, unless expands to (if (not <expr>) <actions>), and the
         * first bug was that b-unless did not recognize not, because it was
         * now named b-not, which was fixed by evaluating the resulting
         * expansion in the original syntax environment, the second problem
         * was that now b-unless operated in an isolated environment and
         * couldn't see the definition of dont-wanna-print, so it would
         * report it was not found, and this was fixed by merging the
         * original syntax environment with the current runtime environment
         * (or, at least, setting the runtime environment as the current
         * parent environment).
         */
        continue;
      }
    }

    return invoke_with_trace(car(p), cdr(p), e);
  } // for (;;) ...
} // eval()

} // extern "C"
