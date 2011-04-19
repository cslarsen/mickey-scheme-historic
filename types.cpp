#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "types.h"

inline static int empty(const char* s)
{
  return s==NULL || *s=='\0';
}

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
  // TODO: Correct code, should be in form "[+-]?[0-9]+"
  return !empty(s) && all(s, isdigit);
}

bool isstring(const char* s)
{
  // TODO: Correct code to allow for escaping quotes, etc
  return !empty(s) && !empty(s+1) // at least `""`
    && s[0]=='"' && s[strlen(s)-1]=='"'
    && count(s+1, isquote)==1;
}

bool isatom(const char* s)
{
  return isalpha(s[0]) && (empty(s+1) ? true : all(s+1, isalnum));
}
