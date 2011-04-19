#include "parser.h"

cons_t* parse_list(environment_t *env)
{
  cons_t *p = NULL;
  const char *token;

  while ( (token = get_token()) != NULL ) {
    if ( *token == ')' )
      break;
    else if ( *token == '(' )
      p = append(p, list(symbol(token+1, env), parse_list(env)));
    else
      p = append(p, list(symbol(token, env)));
  }

  return p;
}

program_t* parse(const char *program)
{
  set_source(program);

  program_t *p = new program_t();
  p->globals = new environment_t();
  p->root = cdr(parse_list(p->globals));
  return p;
}
