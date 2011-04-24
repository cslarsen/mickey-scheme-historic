#include <ctype.h> // toupper
#include <stdarg.h>
#include "util.h"

std::string toupper(const std::string& str)
{
  const char* s = str.c_str();
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
  return strcpy((char*)malloc((s? strlen(s) : 0 ) + 1), s? s : "");
}

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
    case '"': *t = '\"'; break;
    }

    strcpy(t+1, t+2); // shift left
  }

  return p;
}

std::string encode_str(const char* s)
{
  // TODO: Make this table-based or something
  std::string r;

  for ( ; *s; ++s ) {
    switch ( *s ) {
    default:   r += *s; break;
    case '\n': r += "\\n"; break;
    case '\r': r += "\\r"; break;
    case '\t': r += "\\t"; break;
    case '\\': r += "\\"; break;
    }
  }

  return r;
}
