#include <stdexcept>
#include "eval.h"
#include "util.h"
#include "primops.h"
#include "primitives.h"

cons_t* eval_print(cons_t* p, int indent)
{
  if ( p == NULL ) {
    printf(" <null>\n");
    return p;
  }

  if ( indent > 0 )
    printf(" ");

  switch ( p->type ) {
  case INTEGER: printf("integer %d\n", p->integer); break;
  case CLOSURE: printf("closure\n"); break;
  case SYMBOL: printf("symbol '%s'", p->symbol->name.c_str()); break;
  case STRING: printf("string '%s'", p->string); break;
  case U8VECTOR: printf("vector"); break;
  case CONTINUATION: printf("continuation"); break;
  case NIL: printf("nil"); break;
  case PAIR: printf("pair"); break;
  default: printf("unknown"); break;
  }
  printf("\n");

  if ( p->type == PAIR ) {
    for ( int n=0; n<indent; ++n )
      printf(" ");
    printf("car:");

    eval_print(p->car, 1+indent);

    for ( int n=0; n<indent; ++n )
      printf(" ");
    printf("cdr:");

    eval_print(p->cdr, 1+indent);
  }

  return p;
}

lambda_t lookup_lambda(symbol_t *s)
{
  // TODO: Put into table
  std::string n = !s? "" : toupper(s->name.c_str());

  if ( n == "DISPLAY" ) return defun_print;
  if ( n == "STRING-APPEND" ) return defun_strcat;
  if ( n == "+" ) return defun_add;
  if ( n == "*" ) return defun_mul;

  return NULL;
}

cons_t* eval(cons_t* p)
{
  if ( p == NULL )
    return p;

  if ( p->type == PAIR ) {
    eval(car(p));
    eval(cdr(p));
  }

  if ( p->type == SYMBOL ) {
    // if we just came from a pair, this SYMBOL is a function,
    // otherwise it's a lambda/function, so exec (TODO)
    lambda_t f = lookup_lambda(p->symbol); // TODO: use environment

// TODO: Throw
//    if ( f == NULL )
//     throw std::runtime_error(format("Unknown definition '%s'", p->symbol->name.c_str()));

    if ( f != NULL )
      return f(cdr(p));
  }

  return p;
}
