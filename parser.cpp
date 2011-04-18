#include "parser.h"

environment_t globals; 

cons_t* parse_list()
{
  cons_t *p = NULL;
  const char *token;

  while ( (token = get_token()) != NULL ) {
    if ( *token == ')' )
      break;
    else if ( *token == '(' )
      p = append(p, list(symbol(token+1, &globals), parse_list()));
    else
      p = append(p, list(symbol(token, &globals)));
  }

  return p;
}

cons_t* parse(const char *program)
{
  set_source(program);
  return cdr(parse_list());
}
