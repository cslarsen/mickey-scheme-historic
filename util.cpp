#include <ctype.h> // toupper
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
  while ( l!=0 && (s[l-1]=='\n' || s[l-1]=='\r' || s[l-1]==' ' || s[l-1] == '\t' ) ) {
    s[--l] = '\0';
  }
  return s;
}

std::string to_s(int n)
{
  char buf[32];
  sprintf(buf, "%d", n);
  return std::string(buf);
}
