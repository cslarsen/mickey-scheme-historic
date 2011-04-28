#include <stdexcept>
#include <vector>
#include <readline/readline.h>
#include "heap.h"
#include "cons.h"
#include "eval.h"
#include "primops.h"
#include "primitives.h"
#include "print.h"
#include "util.h"
#include "file_io.h"

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
  e->defun("load", defun_load);
  e->defun("debug", defun_debug);
  e->defun("exit", defun_exit);
  e->defun("version", defun_version);

  // cons and friends
  e->defun("cons", defun_cons);
  e->defun("car", defun_car);
  e->defun("cdr", defun_cdr);
  e->defun("caar", defun_caar);
  e->defun("cadr", defun_cadr);
  e->defun("cdar", defun_cdar);
  e->defun("cddr", defun_cddr);
  e->defun("append", defun_append);

  // predicates
  e->defun("atom?", defun_atomp);
  e->defun("symbol?", defun_symbolp);
  e->defun("integer?", defun_integerp);
  e->defun("null?", defun_nullp);
  e->defun("pair?", defun_pairp);
  e->defun("list?", defun_listp);
  e->defun("procedure?", defun_procedurep);
  e->defun("char?", defun_charp);
  e->defun("boolean?", defun_booleanp);
  e->defun("vector?", defun_vectorp);

  e->defun("length", defun_length);
}

cons_t* defun_print(cons_t *p, environment_t* env)
{
  for ( ; !nullp(p); p = cdr(p) ) {
    if ( !listp(p) )
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
    if ( !listp(p) )
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
    else if ( listp(p) ) {
      cons_t *res = eval(car(p), env);
      if ( integerp(res) )
        sum += res->integer; // or else, thow (TOWO)
    } else
      throw std::runtime_error("Cannot add integer with: " + sprint(p));
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
    else if ( listp(p) ) {
      cons_t *res = eval(car(p), env);
      if ( integerp(res) )
        product *= res->integer; // else, throw (TODO)
    } else
      throw std::runtime_error("Cannot multiply integer with: " + sprint(p));
  }

  return integer(product);
}

cons_t* defun_begin(cons_t* p, environment_t *env)
{
  // execute in order of appearance
  cons_t *r = NULL;

  for ( ; !nullp(p); p = cdr(p) )
    if ( !listp(p) )
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
    else if ( listp(p) )
      s += sprint(eval(car(p), env));
  }

  return string(s.c_str());
}

cons_t* defun_list(cons_t* p, environment_t *env)
{
  cons_t *l = NULL;

  for ( ; !nullp(p); p = cdr(p))
    if ( !listp(p) )
      l = append(l, p);
    else
      l = append(l, cons(eval(car(p), env)));

  // `(list)` should return `()'
  return nullp(l) ? list(NULL) : l;
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

cons_t* defun_load(cons_t *filename, environment_t *env)
{
  if ( !stringp(car(filename)) )
    throw std::runtime_error("First argument to (load) must be a string");

  program_t *p = parse(slurp(open_file(car(filename)->string)).c_str(), env);

  // When reading from disk, we implicitly wrap it all in (begin ...)
  p->root = cons(env->lookup("begin"), p->root);

  eval(p);
  return nil();
}

cons_t* defun_debug(cons_t *p, environment_t *env)
{
  std::string s;

  s = format("adr=%-11p type=%-7s", p, to_s(type_of(p)).c_str());

  switch ( type_of(p) ) {
  case NIL: break;
  case CHAR:
    s += format(" value=%d", p->character);
    break;
  case BOOLEAN:
    s += format(" value=%s", p->integer? "#t" : "#f");
    break;
  case INTEGER:
    s += format(" value=%d", p->integer);
    break;
  case CLOSURE:
    s += format(" function=%p environment=%p",
           p->closure->function,
           p->closure->environment);
    break;
  case PAIR:
    s += format(" car=%p cdr=%p", p->car, p->cdr);
    break;
  case SYMBOL:
    s += format(" name='%s'", p->symbol->name.c_str());
    break;
  case STRING:
    s += format(" value='%s'", p->string);
    break;
  case VECTOR:
    break;
  case CONTINUATION:
    break;
  }

  s += "\n";

  if ( type_of(p) == PAIR ) {
    s += defun_debug(car(p), env)->string;
    s += defun_debug(cdr(p), env)->string;
  }

  return string(s.c_str());
}

cons_t* defun_exit(cons_t* p, environment_t*)
{
  exit(integerp(car(p))? car(p)->integer : 0);
  return NULL;
}

cons_t* defun_cons(cons_t* p, environment_t* e)
{
  return cons(car(p), cadr(p));
}

cons_t* defun_car(cons_t* p, environment_t* env)
{
  return car(eval(car(p), env));
}

cons_t* defun_cdr(cons_t* p, environment_t* env)
{
  /*
   * NOTE:  We have a special (and potentially UGLY) case
   *        of doing "(cdr (list 1))" which should give "()",
   *        so we explicitly check for it here, although we
   *        probably SHOULD NOT (TODO).
   */
  cons_t *r = cdr(car(eval(p, env)));
  return r? r : cons(NULL);
}

cons_t* defun_caar(cons_t* p, environment_t* e)
{
  return car(defun_car(p, e));
}

cons_t* defun_cadr(cons_t* p, environment_t* e)
{
  return car(defun_cdr(p, e));
}

cons_t* defun_cdar(cons_t* p, environment_t* e)
{
  return cdr(defun_car(p, e));
}

cons_t* defun_cddr(cons_t* p, environment_t* e)
{
  // see defun_cdr for UGLINESS
  cons_t *r = cdr(defun_cdr(p, e));
  return r? r : cons(NULL); // <= UGLY PUGLY
}

cons_t* defun_append(cons_t* p, environment_t*)
{
  return append(car(p), cadr(p));
}

cons_t* defun_atomp(cons_t* p, environment_t* env)
{
  return boolean(atomp(eval(car(p), env)));
}

cons_t* defun_symbolp(cons_t* p, environment_t* env)
{
  return boolean(symbolp(eval(car(p), env)));
}

cons_t* defun_integerp(cons_t* p, environment_t* env)
{
  return boolean(integerp(eval(car(p), env)));
}

cons_t* defun_nullp(cons_t* p, environment_t* env)
{
  return boolean(nullp(eval(car(p), env)));
}

cons_t* defun_pairp(cons_t* p, environment_t* env)
{
  /*
   * TODO: This works by BRUTE FORCE;
   *       fix the evaluator (lookup+eval evlis there)
   *       and fix the parser (don't cons the car before
   *       returning)
   */
  return boolean(pairp(eval(car(p), env)));
}

cons_t* defun_listp(cons_t* p, environment_t* env)
{
  return boolean(listp(eval(car(p), env)));
}

cons_t* defun_procedurep(cons_t* p, environment_t* e)
{
  return boolean(closurep(eval(car(p), e)));
}

cons_t* defun_vectorp(cons_t* p, environment_t* e)
{
  return boolean(vectorp(car(eval(p, e))));
}

cons_t* defun_charp(cons_t* p, environment_t* e)
{
  return boolean(charp(car(eval(p, e))));
}

cons_t* defun_booleanp(cons_t* p, environment_t* e)
{
  return boolean(booleanp(car(eval(p, e))));
}

cons_t* defun_version(cons_t*, environment_t*)
{
  cons_t *v = list(string("Mickey Scheme (C) 2011 Christian Stigen Larsen\n"));
  v = append(v, cons(string(format("Using Readline %d.%d\n", (rl_readline_version & 0xFF00) >> 8, rl_readline_version & 0x00FF).c_str())));
  v = append(v, cons(string(format("Using Boehm-Demers-Weiser GC %d.%d\n", GC_VERSION_MAJOR, GC_VERSION_MINOR).c_str())));
  v = append(v, cons(string(format("Compiler version: %s\n", __VERSION__).c_str())));
  return v;
}

cons_t* defun_length(cons_t* p, environment_t* env)
{
  int n = 0;

  // again, eval should be handled by evlis in eval()
  p = eval(car(p), env);

  if ( !listp(p) )
    throw std::runtime_error("First argument to (length) must be a list");

  while ( !nullp(p) ) {
    p = cdr(p);
    ++n;
  } 

  return integer(n);
}
