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

cons_t* proc_print(cons_t *p, environment_t* env)
{
  for ( ; !nullp(p); p = cdr(p) ) {
    if ( !listp(p) )
      printf("%s", to_s(p).c_str());
    else
      proc_print(car(p), env);
  }

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
    if ( !listp(p) )
      s += to_s(p);
    else
      s += proc_strcat(car(p), env)->string;

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
  return caar(p);
}

cons_t* proc_cdr(cons_t* p, environment_t* env)
{
  /*
   * NOTE:  We have a special (and potentially UGLY) case
   *        of doing "(cdr (list 1))" which should give "()",
   *        so we explicitly check for it here, although we
   *        probably SHOULD NOT (TODO).
   */
  cons_t *r = cdar(p);
  return r? r : cons(NULL);
}

cons_t* proc_caar(cons_t* p, environment_t* e)
{
  return car(caar(p));
}

cons_t* proc_cadr(cons_t* p, environment_t* e)
{
  return car(proc_cdr(p, e));
}

cons_t* proc_cdar(cons_t* p, environment_t* e)
{
  return cdr(proc_car(p, e));
}

cons_t* proc_cddr(cons_t* p, environment_t* e)
{
  // see proc_cdr for UGLINESS
  cons_t *r = cdr(proc_cdr(p, e));
  return r? r : cons(NULL); // <= UGLY PUGLY
}

cons_t* proc_append(cons_t* p, environment_t*)
{
  return append_non_mutable(car(p), cadr(p));
}

cons_t* proc_atomp(cons_t* p, environment_t* env)
{
  return boolean(atomp(car(p)));
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
  /*
   * TODO: This works by BRUTE FORCE;
   *       fix the evaluator (lookup+eval evlis there)
   *       and fix the parser (don't cons the car before
   *       returning)
   */
  return boolean(pairp(car(p)));
}

cons_t* proc_listp(cons_t* p, environment_t* env)
{
  return boolean(listp(car(p)));
}

cons_t* proc_procedurep(cons_t* p, environment_t* e)
{
  return boolean(closurep(car(p)));
}

cons_t* proc_vectorp(cons_t* p, environment_t* e)
{
  return boolean(vectorp(car(p)));
}

cons_t* proc_charp(cons_t* p, environment_t* e)
{
  return boolean(charp(car(p)));
}

cons_t* proc_booleanp(cons_t* p, environment_t* e)
{
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
     names = append_non_mutable(names, list(caar(n)));
    values = append_non_mutable(values, list(car(cdar(n))));
  }

  /*
   * Build lambda expression and return it, eval will eval it :)
   * (or we could call make_closure here):
   *
   * ((lambda (<names>) <body>) <values>)
   *
   */
  return cons(cons(symbol("lambda", e),
          cons(names, cons(body))), values);
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
    return append_non_mutable(r, action);
  else 
    return append_non_mutable(r,
          cons(symbol("if", e),
            cons(test,
              cons(action,
                cons(otherwise)))));
}

cons_t* proc_number_to_string(cons_t* p, environment_t* e)
{
  assert_number(car(p));
  assert_length(p, 1);
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

named_function_t exports_base[] = {
  {"*", proc_mul},
  {"+", proc_add},
  {"-", proc_sub},
  {"->string", proc_to_string},
  {"/", proc_divf},
  {"//", proc_div},
  {"<", proc_less},
  {"=", proc_eqintp},
  {">", proc_greater},
  {"abs", proc_abs},
  {"and", proc_and},
  {"append", proc_append},
  {"atom?", proc_atomp},
  {"backtrace", proc_backtrace},
  {"boolean?", proc_booleanp},
  {"caar", proc_caar},
  {"cadr", proc_cadr},
  {"car", proc_car},
  {"cdar", proc_cdar},
  {"cddr", proc_cddr},
  {"cdr", proc_cdr},
  {"char?", proc_charp},
  {"closure-source", proc_closure_source},
  {"cons", proc_cons},
  {"debug", proc_debug},
  {"display", proc_print},
  {"eq?", proc_eqp},
  {"equal?", proc_equalp},
  {"exit", proc_exit},
  {"file-exists?", proc_file_existsp},
  {"float?", proc_decimalp},
  {"integer?", proc_integerp},
  {"length", proc_length},
  {"list", proc_list},
  {"list?", proc_listp},
  {"load", proc_load},
  {"newline", proc_newline},
  {"not", proc_not},
  {"null?", proc_nullp},
  {"number->string", proc_number_to_string},
  {"or", proc_or},
  {"pair?", proc_pairp},
  {"procedure?", proc_procedurep},
  {"reverse", proc_reverse},
  {"string-append", proc_strcat},
  {"symbol?", proc_symbolp},
  {"type-of", proc_type_of},
  {"vector?", proc_vectorp},
  {"version", proc_version},
  {"write", proc_print},
  {"xor", proc_xor},
  {"zero?", proc_zerop},
  {NULL, NULL}};

