#include <stdexcept>
#include <vector>
#include "eval.h"
#include "primops.h"
#include "primitives.h"
#include "print.h"

// TODO: Put into environment, in fact, share this struct with symbol
static std::map<symbol_t*, lambda_t> functions;

lambda_t lookup_lambda(symbol_t *s)
{
  if ( functions.find(s) != functions.end() )
    return functions[s];

  return NULL;
}

void defun(symbol_t *s, lambda_t f)
{
  functions[s] = f;
}

void load_default_defs(environment_t *e)
{
  defun(symbol_t::create_symbol("begin", e), defun_begin);
  defun(symbol_t::create_symbol("display", e), defun_print);
  defun(symbol_t::create_symbol("string-append", e), defun_strcat);
  defun(symbol_t::create_symbol("+", e), defun_add);
  defun(symbol_t::create_symbol("*", e), defun_mul);
  defun(symbol_t::create_symbol("->string", e), defun_to_string);
}

// TODO: Move to another place
std::string to_s(enum type_t type)
{
  switch ( type ) {
  default:       return "<?>";      break;
  case NIL:      return "nil";      break;
  case INTEGER:  return "integer";  break;
  case CLOSURE:  return "cloure";   break;
  case PAIR:     return "pair";     break;
  case SYMBOL:   return "symbol";   break;
  case STRING:   return "string";   break;
  case U8VECTOR: return "U8VECTOR"; break;
  case CONTINUATION: return "continuation"; break;
  }
}

// TODO: Move to another place
std::string to_s(cons_t *p)
{
  switch ( type_of(p) ) {
  default:       return "<?>";
  case NIL:      return "<nil>";
  case INTEGER:  return to_s(p->integer);
  case CLOSURE:  return format("<closure %p>", p->closure);
  case PAIR:     return to_s(car(p)) + " . " + to_s(cdr(p));
  case SYMBOL:   return p->symbol->name;
  case STRING:   return p->string;
  case U8VECTOR: return format("<u8vector %p>", p->u8vector);
  case CONTINUATION: return format("<continuation %p>", p->continuation);
  }
}

cons_t* defun_print(cons_t *p)
{
  for ( ; !nullp(p); p = cdr(p) )
    if ( !pairp(p) )
      printf("%s", to_s(p).c_str());
    else
      defun_print(eval(car(p)));

  return string("");
}

cons_t* defun_strcat(cons_t *p)
{
  std::string s;

  for ( ; !nullp(p); p = cdr(p) )
    if ( !pairp(p) )
      s += to_s(p);
    else
      s += defun_strcat(eval(car(p)))->string;

  return string(s.c_str());
}

cons_t* defun_add(cons_t *p)
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
      cons_t *res = eval(car(p));
      if ( integerp(res) )
        sum += res->integer; // or else, thow (TOWO)
    } else
      throw std::runtime_error("Cannot add integer and " + to_s(type_of(p)));
  }

  return integer(sum);
}

cons_t* defun_mul(cons_t *p)
{
  // Identity; see defun_add
  int product = 1;

  for ( ; !nullp(p); p = cdr(p)) {
    if ( integerp(p) )
      product *= p->integer;
    else if ( pairp(p) ) {
      cons_t *res = eval(car(p));
      if ( integerp(res) )
        product *= res->integer; // else, throw (TODO)
    }
    // incompatible types; throw error or something
  }

  return integer(product);
}

cons_t* defun_begin(cons_t* p)
{
  // execute in order of appearance
  cons_t *r = NULL;

  for ( ; !nullp(p); p = cdr(p) )
    if ( !pairp(p) )
      r = append(r, eval(p));
    else
      r = append(r, eval(car(p)));

  return r;
}

cons_t* defun_to_string(cons_t* p)
{
  return string(sprint(p).c_str());
}
