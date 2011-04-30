#include <stdexcept>
#include "parser.h"
#include "types.h"
#include "util.h"

cons_t* type_convert(const char* token, environment_t* env)
{
  if ( isfloat(token) )
    return decimal(to_f(token));

  if ( isinteger(token) )
    return integer(to_i(token));

  if ( isstring(token) )
    return string(decode_literal_string(token));

  if ( isatom(token) )
    return symbol(token, env);

  if ( isbool(token) )
    return boolean(to_b(token));

  if ( !token || !*token )
    return nil();

  // probably a function called "+" or something
  return symbol(token, env);
}

cons_t* parse_list(environment_t *env)
{
  cons_t *p = NULL;
  const char *token;

  while ( (token = get_token()) != NULL ) {
    if ( *token == ')' )
      break;

    if ( *token == '(' ) {
      cons_t *obj = type_convert(token+1, env);

      if ( !nullp(obj) ) 
        p = append(p, list(obj, parse_list(env)));
      else
        p = append(p, list(parse_list(env)));
    } else {
      cons_t *obj = type_convert(token, env);
      p = append(p, list(obj));
    }
  }

  return p;
}

program_t* parse(const char *program, environment_t *env)
{
  set_source(program);

  if ( env == NULL )
    throw std::runtime_error("parse: null environment");

  program_t *p = new program_t();
  p->globals = env;
  p->root = parse_list(p->globals);
  return p;
}
