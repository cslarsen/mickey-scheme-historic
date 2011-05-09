#include <math.h>
#include <sys/stat.h>
#include <stdexcept>
#include <vector>

#ifdef USE_READLINE
# include <readline/readline.h>
#endif

#include "mickey.h"
#include "options.h"
#include "heap.h"
#include "cons.h"
#include "eval.h"
#include "primops.h"
#include "primitives.h"
#include "print.h"
#include "util.h"
#include "file_io.h"
#include "types.h"
#include "backtrace.h"
#include "module.h"
#include "module_math.h"
#include "assertions.h"

// TODO: Fix this, had to do it because of circular cons/util deps
extern std::string to_s(cons_t*);

static bool file_exists(const char* s)
{
  struct stat st;
  return !stat(s, &st);
}

closure_t* lookup_closure(symbol_t *s, environment_t *env)
{
  cons_t *p = env->lookup(s->name());
  return closurep(p)? p->closure : NULL;
}

void load_default_defs(environment_t *e)
{
  e->defun("display", defun_print);
  e->defun("newline", defun_newline);
  e->defun("write", defun_print);
  e->defun("string-append", defun_strcat);
  e->defun("->string", defun_to_string);
  e->defun("number->string", defun_number_to_string);

  e->defun("-", defun_sub);
  e->defun("+", defun_add);
  e->defun("*", defun_mul);
  e->defun("/", defun_divf);
  e->defun("//", defun_div);
  e->defun("sqrt", defun_sqrt);

  e->defun("eq?", defun_eqp);
  e->defun("equal?", defun_equalp);
  e->defun("=", defun_eqintp);
  e->defun("<", defun_less);
  e->defun(">", defun_greater);

  e->defun("load", defun_load);
  e->defun("debug", defun_debug);
  e->defun("exit", defun_exit);
  e->defun("version", defun_version);
  e->defun("length", defun_length);
  e->defun("closure-source", defun_closure_source);
  e->defun("backtrace", defun_backtrace);
  e->defun("type-of", defun_type_of);

  e->defun("file-exists?", defun_file_existsp);

  // cons and friends
  e->defun("list", defun_list);
  e->defun("cons", defun_cons);
  e->defun("car", defun_car);
  e->defun("cdr", defun_cdr);
  e->defun("caar", defun_caar);
  e->defun("cadr", defun_cadr);
  e->defun("cdar", defun_cdar);
  e->defun("cddr", defun_cddr);
  e->defun("append", defun_append);
  e->defun("reverse", defun_reverse);

  // predicates
  e->defun("atom?", defun_atomp);
  e->defun("symbol?", defun_symbolp);
  e->defun("integer?", defun_integerp);
  e->defun("float?", defun_decimalp);
  e->defun("null?", defun_nullp);
  e->defun("pair?", defun_pairp);
  e->defun("list?", defun_listp);
  e->defun("procedure?", defun_procedurep);
  e->defun("char?", defun_charp);
  e->defun("boolean?", defun_booleanp);
  e->defun("vector?", defun_vectorp);
  e->defun("zero?", defun_zerop);

  e->defun("not", defun_not);
  e->defun("and", defun_and);
  e->defun("or", defun_or);
  e->defun("xor", defun_xor);
  e->defun("abs", defun_abs);

  import(e, exports_math);
}

cons_t* defun_abs(cons_t* p, environment_t*)
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

cons_t* defun_print(cons_t *p, environment_t* env)
{
  for ( ; !nullp(p); p = cdr(p) ) {
    if ( !listp(p) )
      printf("%s", to_s(p).c_str());
    else
      defun_print(car(p), env);
  }

  return nil();
}

cons_t* defun_newline(cons_t*, environment_t*)
{
  printf("\n");
  return nil();
}

cons_t* defun_strcat(cons_t *p, environment_t* env)
{
  std::string s;

  for ( ; !nullp(p); p = cdr(p) )
    if ( !listp(p) )
      s += to_s(p);
    else
      s += defun_strcat(car(p), env)->string;

  return string(s.c_str());
}

cons_t* defun_addf(cons_t *p, environment_t* env)
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
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) )
      sum += i->integer;
    else if ( decimalp(i) )
      // automatically convert; perform rest of computation in floats
      return defun_addf(cons(decimal(sum), p), env);
    else
      throw std::runtime_error("Cannot add integer with " + to_s(type_of(i)) + ": " + sprint(i));
  }

  return integer(sum);
}

static float as_float(cons_t *p, bool convert_strings = false)
{
  if ( type_of(p) == DECIMAL )
    return p->decimal;

  if ( type_of(p) == INTEGER )
    return (float) p->integer;

  if ( convert_strings ) {
    if ( type_of(p) == STRING && isfloat(p->string) )
      return to_f(p->string);

    if ( type_of(p) == STRING && isinteger(p->string) )
      return (float) to_i(p->string);
  }

  throw std::runtime_error("Cannot convert to float: " + sprint(p));
}

static bool whole_numberp(float n)
{
  // Return true if `n` has no decimals, i.e. is "x.0" for a value of x
  // NOTE: Can possible do `(int)n == n` as well, but better to use floor.
  return floor(n) == n;
}

cons_t* defun_sub(cons_t *p, environment_t* env)
{
  if ( length(p) == 0 )
    throw std::runtime_error("No arguments to -");

  float d = as_float(car(p), false);

  // (- x) => -x, instead of +x
  if ( nullp(cdr(p)) )
    d = -d;

  while ( !nullp(p = cdr(p)) )
    d -= as_float(car(p), false);

  return whole_numberp(d) ? integer((int)d) : decimal(d);
}

cons_t* defun_divf(cons_t *p, environment_t *e)
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
  return whole_numberp(q) ? integer((int)q) : decimal(q);
}

cons_t* defun_div(cons_t *p, environment_t *e)
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
    return defun_divf(p, e);
}

cons_t* defun_mulf(cons_t *p, environment_t *env)
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

cons_t* defun_mul(cons_t *p, environment_t *env)
{
  int product = 1;

  for ( ; !nullp(p); p = cdr(p) ) {
    cons_t *i = listp(p)? car(p) : p;

    if ( integerp(i) )
      product *= i->integer;
    else if ( decimalp(i) )
      // automatically convert; perform rest of computation in floats
      return defun_mulf(cons(decimal(product), p), env);
    else
      throw std::runtime_error("Cannot multiply integer with " + to_s(type_of(i)) + ": " + sprint(i));
  }

  return integer(product);
}

cons_t* defun_to_string(cons_t* p, environment_t *env)
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

cons_t* defun_list(cons_t* p, environment_t *env)
{
  return nullp(p) ? list(NULL) : p;
}

cons_t* defun_define(cons_t *p, environment_t *env)
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

static cons_t* begin(cons_t* p, environment_t* e)
{
  return cons(symbol("begin", e), p);
}

cons_t* defun_load(cons_t *filename, environment_t *env)
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
  p->root = begin(p->root, p->globals); 

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
  return caar(p);
}

cons_t* defun_cdr(cons_t* p, environment_t* env)
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

cons_t* defun_caar(cons_t* p, environment_t* e)
{
  return car(caar(p));
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
  return append_non_mutable(car(p), cadr(p));
}

cons_t* defun_atomp(cons_t* p, environment_t* env)
{
  return boolean(atomp(car(p)));
}

cons_t* defun_symbolp(cons_t* p, environment_t* env)
{
  return boolean(symbolp(car(p)));
}

cons_t* defun_integerp(cons_t* p, environment_t* env)
{
  return boolean(integerp(car(p)));
}

cons_t* defun_decimalp(cons_t* p, environment_t* env)
{
  return boolean(decimalp(car(p)));
}

cons_t* defun_nullp(cons_t* p, environment_t* env)
{
  return boolean(nullp(car(p)));
}

cons_t* defun_zerop(cons_t* p, environment_t*)
{
  if ( type_of(car(p)) == INTEGER )
    return boolean(car(p)->integer == 0);

  if ( type_of(car(p)) == DECIMAL )
    return boolean(car(p)->decimal == 0.0);

  return boolean(false);
}

cons_t* defun_pairp(cons_t* p, environment_t* env)
{
  /*
   * TODO: This works by BRUTE FORCE;
   *       fix the evaluator (lookup+eval evlis there)
   *       and fix the parser (don't cons the car before
   *       returning)
   */
  return boolean(pairp(car(p)));
}

cons_t* defun_listp(cons_t* p, environment_t* env)
{
  return boolean(listp(car(p)));
}

cons_t* defun_procedurep(cons_t* p, environment_t* e)
{
  return boolean(closurep(car(p)));
}

cons_t* defun_vectorp(cons_t* p, environment_t* e)
{
  return boolean(vectorp(car(p)));
}

cons_t* defun_charp(cons_t* p, environment_t* e)
{
  return boolean(charp(car(p)));
}

cons_t* defun_booleanp(cons_t* p, environment_t* e)
{
  return boolean(booleanp(car(p)));
}

cons_t* defun_version(cons_t*, environment_t*)
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

cons_t* defun_length(cons_t* p, environment_t*)
{
  return integer(length(car(p)));
}

cons_t* defun_eqp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  return boolean(eqp(car(p), cadr(p)));
}

cons_t* defun_equalp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  return boolean(equalp(car(p), cadr(p)));
}

static float to_float(cons_t* v)
{
  // TODO: Could handle strings via atof()
  assert_number(v);
  return type_of(v)==DECIMAL ? v->decimal : (float) v->integer;
}

cons_t* defun_eqintp(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_number(car(p));
  assert_number(cadr(p));

  cons_t *l = car(p),
         *r = cadr(p);

  return (decimalp(l) || decimalp(r)) ?
    boolean(to_float(l) == to_float(r)) :
    boolean(l->integer == r->integer);
}

cons_t* defun_not(cons_t* p, environment_t*)
{
  assert_length(p, 1);
  return boolean(not_p(p));
}

cons_t* defun_and(cons_t* p, environment_t*)
{
  return boolean(and_p(p));
}

cons_t* defun_or(cons_t* p, environment_t*)
{
  return boolean(or_p(p));
}

cons_t* defun_xor(cons_t* p, environment_t*)
{
  return boolean(xor_p(p));
}

cons_t* defun_sqrt(cons_t* p, environment_t*)
{
  // TODO: If number < 0, return complex root (??)
  switch ( type_of(car(p)) ) {
  default: assert_number(p); return nil(); break;
  case INTEGER: return decimal(sqrt(car(p)->integer));
  case DECIMAL: return decimal(sqrt(car(p)->decimal));
  }
}

cons_t* defun_less(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_number(car(p));
  assert_number(cadr(p));

  float x = (type_of(car(p)) == INTEGER)? car(p)->integer : car(p)->decimal;
  float y = (type_of(cadr(p)) == INTEGER)? cadr(p)->integer : cadr(p)->decimal;

  return boolean(x < y);
}

cons_t* defun_greater(cons_t* p, environment_t*)
{
  assert_length(p, 2);
  assert_number(car(p));
  assert_number(cadr(p));

  float x = (type_of(car(p)) == INTEGER)? car(p)->integer : car(p)->decimal;
  float y = (type_of(cadr(p)) == INTEGER)? cadr(p)->integer : cadr(p)->decimal;

  return boolean(x > y);
}

cons_t* defun_closure_source(cons_t* p, environment_t* e)
{
  assert_type(CLOSURE, car(p));

  closure_t *c = car(p)->closure;

  cons_t *body = c->environment->symbols["__body__"]; // see eval.cpp
  cons_t *args = c->environment->symbols["__args__"];

  cons_t *source = cons(symbol("lambda", e), cons(args, cons(car(body))));
  return source;
}

cons_t* defun_reverse(cons_t* p, environment_t*)
{
  cons_t *r = NULL;

  for ( p = car(p); !nullp(p); p = cdr(p) )
    r = cons(car(p), r);

  return r;
}

cons_t* defun_let(cons_t* p, environment_t* e)
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

cons_t* defun_letstar(cons_t* p, environment_t* e)
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
  cons_t *inner = begin(body, e);

  while ( !nullp(names) && !nullp(values) ) {
    inner = cons(cons(symbol("lambda", e),
      cons(cons(car(names)), cons(inner))), cons(car(values)));

     names = cdr(names);
    values = cdr(values);
  }

  return inner;
}

cons_t* defun_backtrace(cons_t*, environment_t*)
{
  backtrace();
  return nil();
}

cons_t* defun_type_of(cons_t* p, environment_t* e)
{
  return symbol(to_s(type_of(car(p))).c_str(), e);
}

cons_t* defun_cond(cons_t* p, environment_t* e)
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

  cons_t *otherwise = defun_cond(p, e);

  if ( symbolp(test) && test->symbol->name() == "else" )
    return append_non_mutable(r, action);
  else 
    return append_non_mutable(r,
          cons(symbol("if", e),
            cons(test,
              cons(action,
                cons(otherwise)))));
}

cons_t* defun_number_to_string(cons_t* p, environment_t* e)
{
  assert_number(car(p));
  assert_length(p, 1);
  return defun_to_string(p, e);
}

cons_t* defun_set_car(cons_t* p, environment_t* e)
{
  assert_type(SYMBOL, car(p));
  std::string name = car(p)->symbol->name();
  e->lookup(name)->car = cadr(p);
  return nil();
}

cons_t* defun_set_cdr(cons_t* p, environment_t* e)
{
  assert_type(SYMBOL, car(p));
  std::string name = car(p)->symbol->name();
  e->lookup(name)->cdr = cadr(p);
  return nil();
}

cons_t* defun_file_existsp(cons_t* p, environment_t*)
{
  return boolean(file_exists(car(p)->string));
}
