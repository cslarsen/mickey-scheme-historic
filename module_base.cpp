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

#include <math.h>
#include "mickey.h" // VERSION
#include "cons.h" // to_s(cons_t*)
#include "module.h"
#include "module_base.h"
#include "primops.h"
#include "assertions.h"
#include "print.h"   // sprint
#include "file_io.h" // file_exists
#include "options.h" // global_opts
#include "backtrace.h"
#include "eval.h"

// For version printing
#ifdef USE_READLINE
# include <readline/readline.h>
#endif

cons_t* proc_abs(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( decimalp(car(p)) ) {
    float n = car(p)->decimal;
    return decimal(n<0.0? -n : n);
  }

  int n = car(p)->integer;
  return integer(n<0? -n : n);
}

cons_t* proc_display(cons_t *p, environment_t* env)
{
  for ( ; !nullp(p); p = cdr(p) ) {
    printf("%s", print(car(p)).c_str());
  }

  return nil();
}

cons_t* proc_write(cons_t *p, environment_t* env)
{
  for ( ; !nullp(p); p = cdr(p) )
    printf("%s", sprint(car(p)).c_str());

  return nil();
}

cons_t* proc_newline(cons_t*, environment_t*)
{
  printf("\n");
  return nil();
}

cons_t* proc_strcat(cons_t *p, environment_t* env)
{
  std::string s;

  for ( ; !nullp(p); p = cdr(p) )
    s += print(car(p)).c_str();

  return string(s.c_str());
}

cons_t* proc_addf(cons_t *p, environment_t* env)
{
  float sum = 0.0;

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) )
      sum += (float) i->integer;
    else if ( decimalp(i) )
      sum += i->decimal;
    else
      throw std::runtime_error("Cannot add decimal with " + to_s(type_of(i)) + ": " + sprint(i));
  }

  return decimal(sum);
}

cons_t* proc_add(cons_t *p, environment_t* env)
{
  /*
   * Integers have an IDENTITY, so we can do this,
   * but a more correct approach would be to take
   * the value of the FIRST number we find and
   * return that.
   */
  int sum = 0;

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) )
      sum += i->integer;
    else if ( decimalp(i) )
      // automatically convert; perform rest of computation in floats
      return proc_addf(cons(decimal(sum), p), env);
    else
      throw std::runtime_error("Cannot add integer with " + to_s(type_of(i)) + ": " + sprint(i));
  }

  return integer(sum);
}

cons_t* proc_sub(cons_t *p, environment_t* env)
{
  if ( length(p) == 0 )
    throw std::runtime_error("No arguments to -");

  float d = number_to_float(car(p));

  // (- x) => -x, instead of +x
  if ( nullp(cdr(p)) )
    d = -d;

  while ( !nullp(p = cdr(p)) )
    d -= number_to_float(car(p));

  return iswhole(d) ? integer((int)d) : decimal(d);
}

cons_t* proc_divf(cons_t *p, environment_t *e)
{
  assert_length(p, 2);

  cons_t *a = car(p);
  cons_t *b = cadr(p);

  float x = (type_of(a) == DECIMAL)? a->decimal : a->integer;
  float y = (type_of(b) == DECIMAL)? b->decimal : b->integer;

  if ( y == 0.0 )
    throw std::runtime_error("Division by zero");

  // Automatically convert back to int if possible
  float q = x / y;
  return iswhole(q)? integer(static_cast<int>(q)) : decimal(q);
}

cons_t* proc_div(cons_t *p, environment_t *e)
{
  assert_length(p, 2);

  cons_t *a = car(p);
  cons_t *b = cadr(p);

  assert_number(a);
  assert_number(b);

  if ( integerp(a) && integerp(b) ) {
    if ( b->integer == 0 )
      throw std::runtime_error("Division by zero");
    return integer(a->integer / b->integer);
  } else
    return proc_divf(p, e);
}

cons_t* proc_mulf(cons_t *p, environment_t *env)
{
  float product = 1.0;

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) )
      product *= (float) i->integer;
    else if ( decimalp(i) )
      // automatically convert; perform rest of computation in floats
      product *= i->decimal;
    else
      throw std::runtime_error("Cannot multiply integer with " + to_s(type_of(i)) + ": " + sprint(i));
  }

  return decimal(product);
}

cons_t* proc_mul(cons_t *p, environment_t *env)
{
  int product = 1;

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) )
      product *= i->integer;
    else if ( decimalp(i) )
      // automatically convert; perform rest of computation in floats
      return proc_mulf(cons(decimal(product), p), env);
    else
      throw std::runtime_error("Cannot multiply integer with " + to_s(type_of(i)) + ": " + sprint(i));
  }

  return integer(product);
}

cons_t* proc_to_string(cons_t* p, environment_t *env)
{
  std::string s;

  for ( ; !nullp(p); p = cdr(p)) {
    if ( listp(car(p)) )
      s += print(car(p));
    else
      s += to_s(car(p));
  }

  return string(s.c_str());
}

cons_t* proc_list(cons_t* p, environment_t *env)
{
  return nullp(p) ? list(NULL) : p;
}

cons_t* proc_define(cons_t *p, environment_t *env)
{
  // (define <name> <body>)
  assert_type(SYMBOL, car(p));
  cons_t *name = car(p);
  cons_t *body = cadr(p);

  if ( name->symbol->name().empty() )
    throw std::runtime_error("Cannot define with empty variable name"); // TODO: Even possible?

  env->define(name->symbol->name(), body);
  return nil();
}

cons_t* proc_load(cons_t *filename, environment_t *env)
{
  assert_type(STRING, car(filename));
  
  program_t *p;

  // read from stdin?
  if ( !strcmp(car(filename)->string, "-") )
    p = parse(slurp(stdin).c_str(), env);
  else {
    // first try filename without include path
    std::string path = car(filename)->string;

    // no cigar? try include path
    if ( !file_exists(path.c_str()) )
      path = format("%s/%s",
               global_opts.include_path,
               car(filename)->string);

    p = parse(slurp(open_file(path.c_str())).c_str(), env);
  }

  // When reading from file, we implicitly wrap it all in (begin ...)
  p->root = proc_begin(p->root, p->globals); 

  eval(p);
  return nil();
}

cons_t* proc_debug(cons_t *p, environment_t *env)
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
  case DECIMAL:
    s += format(" value=%f", p->decimal);
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
    s += format(" name='%s'", p->symbol->name().c_str());
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
    s += proc_debug(car(p), env)->string;
    s += proc_debug(cdr(p), env)->string;
  }

  return string(s.c_str());
}

cons_t* proc_exit(cons_t* p, environment_t*)
{
  exit(integerp(car(p))? car(p)->integer : 0);
  return NULL;
}

cons_t* proc_cons(cons_t* p, environment_t* e)
{
  return cons(car(p), cadr(p));
}

cons_t* proc_car(cons_t* p, environment_t* env)
{
  assert_length(p, 1);
  assert_type(PAIR, p);
  return car(car(p));
}

cons_t* proc_cdr(cons_t* p, environment_t* env)
{
  /*
   * NOTE:  We have a special (and potentially UGLY) case
   *        of doing "(cdr (list 1))" which should give "()",
   *        so we explicitly check for it here, although we
   *        probably SHOULD NOT (TODO).
   */
  assert_length(p, 1);
  assert_type(PAIR, p);
  cons_t *r = cdr(car(p));
  return nullp(r)? list(NULL) : r;
}

cons_t* proc_caar(cons_t* p, environment_t* e)
{
  return car(proc_car(p,e));
}

cons_t* proc_caaar(cons_t* p, environment_t* e)
{
  return caar(proc_car(p,e));
}

cons_t* proc_cadr(cons_t* p, environment_t* e)
{
  return car(proc_cdr(p,e));
}

cons_t* proc_caadr(cons_t* p, environment_t* e)
{
  return caar(proc_cdr(p,e));
}

cons_t* proc_cdar(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  assert_type(PAIR, p);
  cons_t *r = cdar(car(p));
  return nullp(r)? list(NULL) : r;
}

cons_t* proc_cddr(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  assert_type(PAIR, p);
  cons_t *r = cddr(car(p));
  return nullp(r)? list(NULL) : r;
}

cons_t* proc_append(cons_t* p, environment_t*)
{
  return append(car(p), cadr(p));
}

cons_t* proc_symbolp(cons_t* p, environment_t* env)
{
  return boolean(symbolp(car(p)));
}

cons_t* proc_integerp(cons_t* p, environment_t* env)
{
  return boolean(integerp(car(p)));
}

cons_t* proc_decimalp(cons_t* p, environment_t* env)
{
  return boolean(decimalp(car(p)));
}

cons_t* proc_nullp(cons_t* p, environment_t* env)
{
  return boolean(nullp(car(p)));
}

cons_t* proc_zerop(cons_t* p, environment_t*)
{
  if ( type_of(car(p)) == INTEGER )
    return boolean(car(p)->integer == 0);

  if ( type_of(car(p)) == DECIMAL )
    return boolean(car(p)->decimal == 0.0);

  return boolean(false);
}

cons_t* proc_pairp(cons_t* p, environment_t* env)
{
  assert_length(p, 1);
  return boolean(pairp(car(p)));
}

cons_t* proc_listp(cons_t* p, environment_t* env)
{
  assert_length(p, 1);
  return boolean(listp(car(p)));
}

cons_t* proc_numberp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(numberp(car(p)));
}

cons_t* proc_stringp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(stringp(car(p)));
}

cons_t* proc_procedurep(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  return boolean(closurep(car(p)));
}

cons_t* proc_vectorp(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  return boolean(vectorp(car(p)));
}

cons_t* proc_charp(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  return boolean(charp(car(p)));
}

cons_t* proc_booleanp(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  return boolean(booleanp(car(p)));
}

cons_t* proc_version(cons_t*, environment_t*)
{
  cons_t *v = list(string(format("%s\n", VERSION).c_str()));

  #ifdef USE_READLINE
  v = append(v, cons(string(format("Using Readline %d.%d\n",
        (rl_readline_version & 0xFF00) >> 8, rl_readline_version & 0x00FF).c_str())));
  #endif

  #ifdef BOEHM_GC
  v = append(v, cons(string(format("Using Boehm-Demers-Weiser GC %d.%d\n", GC_VERSION_MAJOR, GC_VERSION_MINOR).c_str())));
  #endif

  v = append(v, cons(string(format("Compiler version: %s\n", __VERSION__).c_str())));
  return v;
}

cons_t* proc_length(cons_t* p, environment_t*)
{
  return integer(length(car(p)));
}

cons_t* proc_eqp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  return boolean(eqp(car(p), cadr(p)));
}

cons_t* proc_eqvp(cons_t* p, environment_t*)
{
  assert_length(p, 2);

  cons_t *l = car(p),
         *r = cadr(p);

  if ( type_of(l) != type_of(r) )
    return boolean(false);

  switch ( type_of(l) ) {
  case NIL:     return boolean(true);
  case BOOLEAN: return boolean(l == r);
  case SYMBOL:  return boolean(l->symbol->name() == r->symbol->name());
  case INTEGER: // Also make sure both are exact/both inexact (TODO)
                return boolean(l->integer == r->integer); 
  case DECIMAL: // Check both exact/both inexact
                return boolean(l->decimal == r->decimal);
  case CHAR:    return boolean(l->character == r->character);
  case PAIR:    return boolean(nullp(l) && nullp(r)? true : l == r);
  case VECTOR:  return boolean(l == r);
  case STRING:  return boolean(l == r);
  case CLOSURE: // double-check with section 6.1 and 4.1.4 (TODO)
                return boolean(l->closure == r->closure);
  case CONTINUATION:
                return boolean(l->continuation == r->continuation);
  }

  return boolean(false);
}

cons_t* proc_equalp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  return boolean(equalp(car(p), cadr(p)));
}

cons_t* proc_eqintp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_number(car(p));
  assert_number(cadr(p));

  cons_t *l = car(p),
         *r = cadr(p);

  return (decimalp(l) || decimalp(r)) ?
    boolean(number_to_float(l) == number_to_float(r)) :
    boolean(l->integer == r->integer);
}

cons_t* proc_not(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(not_p(p));
}

cons_t* proc_and(cons_t* p, environment_t*)
{
  return boolean(and_p(p));
}

cons_t* proc_or(cons_t* p, environment_t*)
{
  return boolean(or_p(p));
}

cons_t* proc_xor(cons_t* p, environment_t*)
{
  return boolean(xor_p(p));
}

cons_t* proc_less(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_number(car(p));
  assert_number(cadr(p));

  float x = (type_of(car(p)) == INTEGER)? car(p)->integer : car(p)->decimal;
  float y = (type_of(cadr(p)) == INTEGER)? cadr(p)->integer : cadr(p)->decimal;

  return boolean(x < y);
}

cons_t* proc_greater(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_number(car(p));
  assert_number(cadr(p));

  float x = (type_of(car(p)) == INTEGER)? car(p)->integer : car(p)->decimal;
  float y = (type_of(cadr(p)) == INTEGER)? cadr(p)->integer : cadr(p)->decimal;

  return boolean(x > y);
}

cons_t* proc_closure_source(cons_t* p, environment_t* e)
{
  assert_type(CLOSURE, car(p));

  closure_t *c = car(p)->closure;

  cons_t *body = c->environment->symbols["__body__"]; // see eval.cpp
  cons_t *args = c->environment->symbols["__args__"];

  cons_t *source = cons(symbol("lambda", e), cons(args, cons(car(body))));
  return source;
}

cons_t* proc_reverse(cons_t* p, environment_t*)
{
  cons_t *r = NULL;

  for ( p = car(p); !nullp(p); p = cdr(p) )
    r = cons(car(p), r);

  return r;
}

cons_t* proc_let(cons_t* p, environment_t* e)
{
  /*
   * Transform to lambdas:
   *
   * (let ((name-1 value-1)
   *       (name-2 value-2)
   *       (name-n value-n))
   *       <body>)
   *
   * to
   *
   * ((lambda (name-1 name-2 name-n)
   *    <body>) value-1 value-2 value-n)
   *
   */

  cons_t  *body  = cdr(p),
          *names = list(NULL),
         *values = list(NULL);

  for ( cons_t *n = car(p); !nullp(n); n = cdr(n) ) {
     names = append(names, list(caar(n)));
    values = append(values, list(car(cdar(n))));
  }

  /*
   * Build lambda expression and return it, eval will eval it :)
   * (or we could call make_closure here):
   *
   * ((lambda (<names>) <body>) <values>)
   *
   */
  return cons(cons(symbol("lambda", e),
          cons(names, cons(proc_begin(body, e)))), values);
}

cons_t* proc_letstar(cons_t* p, environment_t* e)
{
  /*
   * Transform to nested lambdas:
   *
   * (let* ((name-1 value-1)
   *        (name-2 value-2)
   *        (name-3 value-3))
   *        <body>)
   * to
   *
   * ((lambda (name-1)
   * ((lambda (name-2)
   * ((lambda (name-3)
   *   <body>) value-3))
   *           value-2))
   *           value-1)
   */

  cons_t  *body  = cdr(p),
          *names = list(NULL),
         *values = list(NULL);

  for ( cons_t *n = car(p); !nullp(n); n = cdr(n) ) {
     names = cons(caar(n), names);
    values = cons(cadar(n), values);
  }

  // Work our way outward by constructing lambdas
  cons_t *inner = proc_begin(body, e);

  while ( !nullp(names) && !nullp(values) ) {
    inner = cons(cons(symbol("lambda", e),
      cons(cons(car(names)), cons(inner))), cons(car(values)));

     names = cdr(names);
    values = cdr(values);
  }

  return inner;
}

cons_t* proc_backtrace(cons_t*, environment_t*)
{
  backtrace();
  return nil();
}

cons_t* proc_type_of(cons_t* p, environment_t* e)
{
  return symbol(to_s(type_of(car(p))).c_str(), e);
}

cons_t* proc_cond(cons_t* p, environment_t* e)
{
  /*
   * Transform:
   *
   * (cond ((case-1) action-1)
   *        ((case-2) action-2)
   *        ((case-n) action-n)
   *        (else <else action>))
   *
   * to
   *
   *  (if (case-1) action-1
   *  (if (case-2) action-2
   *  (if (case-n) action-n
   *    <else action>)))
   *
   */

  // The code below is quite messy (FIXME)

  cons_t *r = list(NULL);
  p = cdr(p);

  if ( nullp(p) )
    return r;

  cons_t   *test = caar(p),
         *action = car(cdar(p));

  cons_t *otherwise = proc_cond(p, e);

  if ( symbolp(test) && test->symbol->name() == "else" )
    return append(r, action);
  else 
    return append(r,
          cons(symbol("if", e),
            cons(test,
              cons(action,
                cons(otherwise)))));
}

cons_t* proc_number_to_string(cons_t* p, environment_t* e)
{
  assert_length(p, 1, 2);
  assert_number(car(p));

  int radix = 10;
  if ( !nullp(cadr(p)) ) {
    assert_type(INTEGER, cadr(p));
    radix = cadr(p)->integer;
  }

  // TODO: Implement use of radix
  return proc_to_string(cons(car(p)), e);
}

cons_t* proc_symbol_to_string(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  assert_type(SYMBOL, car(p));
  return proc_to_string(p, e);
}

cons_t* proc_boolean_to_string(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  assert_type(BOOLEAN, car(p));
  return proc_to_string(p, e);
}

cons_t* proc_list_to_string(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  assert_type(PAIR, car(p));
  return proc_to_string(p, e);
}

cons_t* proc_set_car(cons_t* p, environment_t* e)
{
  assert_type(SYMBOL, car(p));
  std::string name = car(p)->symbol->name();
  e->lookup(name)->car = cadr(p);
  return nil();
}

cons_t* proc_set_cdr(cons_t* p, environment_t* e)
{
  assert_type(SYMBOL, car(p));
  std::string name = car(p)->symbol->name();
  e->lookup(name)->cdr = cadr(p);
  return nil();
}

cons_t* proc_file_existsp(cons_t* p, environment_t*)
{
  return boolean(file_exists(car(p)->string));
}

cons_t* proc_begin(cons_t* p, environment_t* e)
{
  return cons(symbol("begin", e), p);
}

cons_t* proc_gteq(cons_t* p, environment_t* e)
{
  return boolean(proc_eqintp(p, e) || proc_greater(p, e));
}

cons_t* proc_lteq(cons_t* p, environment_t* e)
{
  return boolean(proc_eqintp(p, e) || proc_less(p, e));
}

cons_t* proc_assq(cons_t* p, environment_t* e)
{
  assert_length(p, 2);

  cons_t *find = car(p),
        *alist = cadr(p);

  for ( p = alist; !nullp(p); p = cdr(p) )
    if ( proc_eqp(list(find, caar(p)), e)->boolean )
      return car(p);

  // Not found
  return boolean(false);
}

cons_t* proc_assv(cons_t* p, environment_t* e)
{
  assert_length(p, 2);

  cons_t *find = car(p),
        *alist = cadr(p);

  for ( p = alist; !nullp(p); p = cdr(p) )
    if ( proc_eqvp(list(find, caar(p)), e)->boolean )
      return car(p);

  // Not found
  return boolean(false);
}

cons_t* proc_assoc(cons_t* p, environment_t* e)
{
  assert_length(p, 2);

  cons_t *find = car(p),
        *alist = cadr(p);

  for ( p = alist; !nullp(p); p = cdr(p) )
    if ( proc_equalp(list(find, caar(p)), e)->boolean )
      return car(p);

  // Not found
  return boolean(false);
}

cons_t* proc_evenp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));
  return boolean(!(car(p)->integer & 1));
}

cons_t* proc_oddp(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_type(INTEGER, car(p));
  return boolean(car(p)->integer & 1);
}

cons_t* proc_negativep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));
  return boolean(integerp(car(p)) ? car(p)->integer < 0 :
                                    car(p)->decimal < 0);
}

cons_t* proc_positivep(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));
  return boolean(integerp(car(p)) ? car(p)->integer > 0 :
                                    car(p)->decimal > 0);
}

cons_t* proc_round(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( integerp(car(p)) )
    return integer(car(p)->integer);
  else
    return decimal(roundf(car(p)->decimal));
}

cons_t* proc_truncate(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  assert_number(car(p));

  if ( integerp(car(p)) )
    return integer(car(p)->integer);
  else
    return decimal(truncf(car(p)->decimal));
}

cons_t* proc_min(cons_t* p, environment_t*)
{
  assert_length_min(p, 1);
  cons_t *min = car(p);

  while ( !nullp(p) ) {
    assert_number(car(p));

    if ( number_to_float(car(p)) < number_to_float(min) )
      min = car(p);

    p = cdr(p);
  }

  return min;
}

cons_t* proc_max(cons_t* p, environment_t*)
{
  assert_length_min(p, 1);
  cons_t *max = car(p);

  while ( !nullp(p) ) {
    assert_number(car(p));

    if ( number_to_float(car(p)) > number_to_float(max) )
      max = car(p);

    p = cdr(p);
  }

  return max;
}

named_function_t exports_base[] = {
  {"*", proc_mul},
  {"+", proc_add},
  {"-", proc_sub},
  {"/", proc_divf},
  {":closure-source", proc_closure_source},
  {":debug", proc_debug},
  {":exit", proc_exit},
  {":type-of", proc_type_of},
  {":version", proc_version},
  {"<", proc_less},
  {"<=", proc_lteq},
  {"=", proc_eqintp},
  {">", proc_greater},
  {">=", proc_gteq},
  {"abs", proc_abs},
  {"and", proc_and},
  {"append", proc_append},
  {"assoc", proc_assoc},
  {"assq", proc_assq},
  {"assq", proc_assv},
  {"backtrace", proc_backtrace},
  {"boolean->string", proc_boolean_to_string},
  {"boolean?", proc_booleanp},
  {"caaar", proc_caaar},
  {"caadr", proc_caadr},
  {"caar", proc_caar},
  {"cadr", proc_cadr},
  {"car", proc_car},
  {"cdar", proc_cdar},
  {"cddr", proc_cddr},
  {"cdr", proc_cdr},
  {"char?", proc_charp},
  {"cons", proc_cons},
  {"display", proc_display},
  {"eq?", proc_eqp},
  {"equal?", proc_equalp},
  {"eqv?", proc_eqvp},
  {"even?", proc_evenp},
  {"file-exists?", proc_file_existsp},
  {"integer?", proc_integerp},
  {"length", proc_length},
  {"list", proc_list},
  {"list->string", proc_list_to_string},
  {"list?", proc_listp},
  {"load", proc_load},
  {"max", proc_max},
  {"min", proc_min},
  {"negative?", proc_negativep},
  {"newline", proc_newline},
  {"not", proc_not},
  {"null?", proc_nullp},
  {"number->string", proc_number_to_string},
  {"number?", proc_numberp},
  {"odd?", proc_oddp},
  {"or", proc_or},
  {"pair?", proc_pairp},
  {"positive?", proc_positivep},
  {"procedure?", proc_procedurep},
  {"real?", proc_decimalp},
  {"reverse", proc_reverse},
  {"round", proc_round},
  {"string-append", proc_strcat},
  {"string?", proc_stringp},
  {"symbol->string", proc_symbol_to_string},
  {"symbol?", proc_symbolp},
  {"truncate", proc_truncate},
  {"vector?", proc_vectorp},
  {"write", proc_write},
  {"xor", proc_xor},
  {"zero?", proc_zerop},
  {NULL, NULL}
};
