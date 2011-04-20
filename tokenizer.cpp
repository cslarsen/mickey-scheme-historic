#include <stdio.h>
#include <stdlib.h> // NULL
#include <string.h>
#include <ctype.h> // isspace, et al
#include "tokenizer.h"
#include "util.h"

static const char* source = NULL;

void set_source(const char* program)
{
  source = program;
}

static bool string_or_non_delimiter(char ch)
{
  static bool inside_string = false;

  if ( ch == '\"' )
    inside_string = !inside_string;

  return ch!='\0' && ch!='(' && ch!=')'
    && (!inside_string? !isspace(ch) : true);
}

static const char* copy_while(char *dest, const char* src, bool (*while_expr)(char))
{
  while ( while_expr(*src) )
    *dest++ = *src++;

  *dest = '\0';
  return src;
}

const char* get_token()
{
  static char token[256];
  token[0] = token[1] = '\0';

  source = skip_space(source);

  // comment? skip to end of line
  if ( *source == ';' )
    while ( *source != '\n' ) ++source;

  source = skip_space(source);

  if ( char_in(*source, "()") )
    // tokens ( and )
    token[0] = *source++;
  else
    // other tokens
    source = copy_while(token, source, string_or_non_delimiter);

  // emit NULL when finished
  return !empty(token) ? token : NULL;
}
