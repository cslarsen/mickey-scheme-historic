#include <stdexcept>
#include "parser.h"
#include "types.h"

char* decode_literal_string(const char* s)
{
  char *p = copy_str(s+1); // chop /^"/
  p[strlen(p)-1] = '\0'; // chop /"$/
  
  // translate "\n" and such
  for ( char *t = p; *t; ++t ) {
    if ( t[0]!='\\' )
      continue;

    // TODO: do this in a cleaner, nicer way (use tables)
    switch ( t[1] ) {
    default: continue;
    case '\0': return p; break;
    case '\\': *t = '\\'; break;
    case 'n': *t = '\n'; break;
    case 'r': *t = '\r'; break;
    case 't': *t = '\t'; break;
    }

    strcpy(t+1, t+2); // shift left
  }

  return p;
}

cons_t* type_convert(const char* token, environment_t* env)
{
  if ( isinteger(token) )
    return integer(to_i(token));

  if ( isstring(token) )
    return string(decode_literal_string(token));

  if ( isatom(token) )
    return symbol(token, env);

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
      p = append(p, list(obj, parse_list(env)));
    } else {
      cons_t *obj = type_convert(token, env);
      p = append(p, list(obj));
    }
  }

  return p;
}

program_t* parse(const char *program)
{
  set_source(program);

  program_t *p = new program_t();
  p->globals = new environment_t();
  p->root = parse_list(p->globals);
  return p;
}
