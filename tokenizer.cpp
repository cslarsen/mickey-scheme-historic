#include <stdlib.h> // NULL
#include <ctype.h> // isspace, et al
#include "tokenizer.h"

static const char* source = NULL;

void set_source(const char* program)
{
 source = program;
}

const char* get_token()
{
  static char token[256];
  token[0] = '\0';

  // skip whitespace
  while ( isspace(*source) )
    ++source;

  // emit tokens "(" or ")"
  if ( *source == '(' || *source == ')' ) {
    token[0] = *source++;
    token[1] = '\0';
    return token;
  }

  // emit other token
  for ( char *t = token;
        *source && *source != ')' && !isspace(*source);
        *t = '\0' )
  {
    *t++ = *source++;
  }

  // emit NULL when finished
  return *token != '\0' ? token : NULL;
}
