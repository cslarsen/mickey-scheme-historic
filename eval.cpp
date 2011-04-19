#include "eval.h"

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

cons_t* eval(cons_t* p)
{
  return eval_print(p, 0);
}
