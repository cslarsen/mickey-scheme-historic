#include <string>
#include <ctype.h> // toupper(char)

std::string toupper(const char* s)
{
  std::string r;
  while ( *s ) r += toupper(*s++);
  return r;
}

