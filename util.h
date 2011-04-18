#include <string>
#include <ctype.h> // toupper(char)

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
