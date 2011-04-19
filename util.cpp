#include <ctype.h> // toupper
#include <stdarg.h>
#include "util.h"

std::string toupper(const char* s)
{
  std::string r;
  while ( *s ) r += toupper(*s++);
  return r;
}

char* trimr(char* s)
{
  size_t l = strlen(s);

  while ( l!=0 && isspace(s[l-1]) )
    s[--l] = '\0';

  return s;
}

std::string to_s(int n)
{
  char buf[32];
  sprintf(buf, "%d", n);
  return std::string(buf);
}

std::string format(const char* fmt, ...)
{
  char buf[1024] = {'\0'};
  va_list list;
  va_start(list, fmt);
  vsprintf(buf, fmt, list);
  va_end(list);
  return std::string(buf);
}

int empty(const char* s)
{
  return s==NULL || *s=='\0';
}

const char* skip_space(const char* s)
{
  while ( isspace(*s) ) ++s;
  return s;
}

bool char_in(char ch, const char* s)
{
  while ( *s )
    if ( ch == *s++ )
      return true;

  return false;
}

char* copy_str(const char* s)
{
  return strcpy((char*)malloc(strlen(s)+1), s);
}
