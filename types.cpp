#include <stdexcept>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "types.h"
#include "util.h"

static int count(const char *s, int (*check)(int))
{
  int n = 0;
  while ( !empty(s) )
    if ( check(*s++) )
      ++n;
  return n;
}

static int all(const char* s, int (*check)(int))
{
  if ( empty(s) )
    return false;

  while ( !empty(s) )
    if ( !check(*s++) )
      return false;
  return true;
}

static int isquote(int s)
{
  return s == '\"';
}

bool isinteger(const char* s)
{
  int sign = (s[0]=='-' || s[0]=='+');
  return !empty(s) && all(s+sign, isdigit);
}

bool isodd(int n)
{
  return n & 1;
}

bool isstring(const char* s)
{
  // TODO: Correct code to allow for escaping quotes, etc
  return !empty(s) && !empty(s+1) // at least `""`
    && s[0]=='"' && s[strlen(s)-1]=='"'
    && isodd(count(s+1, isquote));
}

bool isatom(const char* s)
{
  return isalpha(s[0]) && (empty(s+1) ? true : all(s+1, isalnum));
}

int to_i(const char* s)
{
  if ( s == NULL )
    throw std::runtime_error("Cannot convert NULL to INTEGER");

  int has_sign = (char_in(*s, "+-"));
  int sign = (s[0]=='-'? -1 : 1);

  return sign * atoi(has_sign + s);
}
