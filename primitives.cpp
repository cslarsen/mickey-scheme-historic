#include <stdexcept>
#include <vector>
#include "cons.h"
#include "eval.h"
#include "primops.h"
#include "primitives.h"
#include "print.h"
#include "util.h"

// TODO: Fix this, had to do it because of circular cons/util deps
extern std::string to_s(cons_t*);

closure_t* lookup_closure(symbol_t *s, environment_t *env)
{
  cons_t *p = env->lookup(s->name.c_str());
  return closurep(p)? p->closure : NULL;
}

void load_default_defs(environment_t *e)
{
  e->defun("begin", defun_begin);
  e->defun("display", defun_print);
  e->defun("string-append", defun_strcat);
  e->defun("+", defun_add);
  e->defun("*", defun_mul);
  e->defun("->string", defun_to_string);
  e->defun("list", defun_list);
  e->defun("define", defun_define);
  e->defun("quote", defun_quote);
}

cons_t* defun_print(cons_t *p, environment_t* env)
{
  for ( ; !nullp(p); p = cdr(p) ) {
    if ( !pairp(p) )
      printf("%s", to_s(p).c_str());
    else
      defun_print(eval(car(p), env), env);
  }

  return nil();
}

cons_t* defun_strcat(cons_t *p, environment_t* env)
{
  std::string s;

  for ( ; !nullp(p); p = cdr(p) )
    if ( !pairp(p) )
      s += to_s(p);
    else
      s += defun_strcat(eval(car(p), env), env)->string;

  return string(s.c_str());
}

cons_t* defun_add(cons_t *p, environment_t* env)
{
  /*
   * Integers have an IDENTITY, so we can do this,
   * but a more correct approach would be to take
   * the value of the FIRST number we find and
   * return that.
   */
  int sum = 0;

  for ( ; !nullp(p); p = cdr(p) ) {
    if ( integerp(p) )
      sum += p->integer;
    else if ( pairp(p) ) {
      cons_t *res = eval(car(p), env);
      if ( integerp(res) )
        sum += res->integer; // or else, thow (TOWO)
    } else
      throw std::runtime_error("Cannot add integer and " + to_s(type_of(p)));
  }

  return integer(sum);
}

cons_t* defun_mul(cons_t *p, environment_t *env)
{
  // Identity; see defun_add
  int product = 1;

  for ( ; !nullp(p); p = cdr(p)) {
    if ( integerp(p) )
      product *= p->integer;
    else if ( pairp(p) ) {
      cons_t *res = eval(car(p), env);
      if ( integerp(res) )
        product *= res->integer; // else, throw (TODO)
    } else
      throw std::runtime_error("Cannot multiply with type " + to_s(type_of(p)));
  }

  return integer(product);
}

cons_t* defun_begin(cons_t* p, environment_t *env)
{
  // execute in order of appearance
  cons_t *r = NULL;

  for ( ; !nullp(p); p = cdr(p) )
    if ( !pairp(p) )
      r = append(r, eval(p, env));
    else
      r = append(r, eval(car(p), env));

  return r;
}

cons_t* defun_to_string(cons_t* p, environment_t *env)
{
  std::string s;

  for ( ; !nullp(p); p = cdr(p)) {
    if ( integerp(p) )
      s += format("%d", p->integer);
    else if ( stringp(p) )
      s += p->string;
    else if ( pairp(p) )
      s += sprint(eval(car(p), env));
  }

  return string(s.c_str());
}

cons_t* defun_list(cons_t* p, environment_t *env)
{
  cons_t *l = NULL;

  for ( ; !nullp(p); p = cdr(p))
    if ( !pairp(p) )
      l = append(l, p);
    else
      l = append(l, cons(eval(car(p), env)));

  return l;
}

cons_t* defun_define(cons_t *p, environment_t *env)
{
  /*
   * Format: (define <name> <body>)
   */

  cons_t *name = car(p);
  cons_t *body = eval(cdr(p), env);

  if ( !symbolp(name) )
    throw std::runtime_error("First argument to (define) must be a symbol");

  /*
   * Ugly hack:  If we do (define a 123) we get car(body)=123 and cdr(body)=nil,
   *             but if we do (define a (list 1 2 3)) we get car(body)=nil and cdr(body)=list(1 2 3)
   *
   *             This is probably due to bugs in the parser, so fixing that is a big TODO.
   */
  if ( nullp(car(body)) && !nullp(cdr(body)) )        //
    body = cdr(body);                                 // <= THIS IS UGLY
  else if ( !nullp(car(body)) && nullp(cdr(body)) )   // <= AND WRONG
    body = car(body);                                 // <= AND MUST BEGONE!
  else if ( !nullp(car(body)) && !nullp(cdr(body)) )  //
    body = cadr(body);

  env->define(name->symbol->name, body);
  return nil();
}

cons_t* defun_quote(cons_t *p, environment_t *env)
{
  // just pass along data without performing eval()
  return nullp(cdr(p)) ? car(p) : cadr(p);
}
