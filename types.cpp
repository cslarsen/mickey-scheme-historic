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

static int isdot(int s)
{
  return s == '.';
}

bool isinteger(const char* s)
{
  int sign = (s[0]=='-' || s[0]=='+');
  return !empty(s) && all(s+sign, isdigit);
}

bool isbool(const char* s)
{
  return s[0]=='#' && (s[1]=='t' || s[1]=='f');
}

bool ischar(const char* s)
{
  /*
   * Format "#\x" for given x.
   */
  return strlen(s) == 3 &&
    s[0]=='#' && s[1]=='\\' && isalpha(s[2]);
}

bool isfloat(const char* s)
{
  // TODO: Make pattern complete
  // Pattern now: [+-]?[0-9]*\.?[0-9]+f?
  size_t dots   = count(s, isdot);
  size_t sign   = (s[0]=='-' || s[0]=='+');
  size_t digits = count(s, isdigit);
  size_t last_f = (s[0]? s[strlen(s)-1] == 'f' : 0);

  /*
   * The parts of a good looking floating point
   * number are: [+-] [0-9] [.] [0-9] [f]
   *
   * Now, the number of digits + MAX ONE dot
   * + MAX ONE trailing 'f' should equal the
   * length of the string -- for a finely formatted
   * floating point number.
   *
   * I know this is a stretch, but I'm NOT going
   * to the pain of installing a regex just to parse
   * a frickin number :-)
   *
   */

  if ( strlen(s+sign) == (digits + (dots==1) + last_f) )
    return digits>0 && (dots==1 || last_f); // or else it's an ordinary integer
  else
    return false;
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

float to_f(const char* s)
{
  return (float) atof(s);
}

int to_i(const char* s)
{
  if ( s == NULL )
    throw std::runtime_error("Cannot convert NULL to INTEGER");

  int has_sign = (char_in(*s, "+-"));
  int sign = (s[0]=='-'? -1 : 1);

  return sign * atoi(has_sign + s);
}

bool to_b(const char* s)
{
  return s[1]=='t'? true : false;
}

char to_char(const char* s)
{
  return s[2];
}
