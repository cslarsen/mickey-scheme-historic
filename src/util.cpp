/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include <ctype.h> // toupper
#include <stdarg.h>
#include "util.h"
#include "types.h"
#include "platform-limits.h"
#include <iostream>

std::string toupper(const std::string& str)
{
  const char* s = str.c_str();
  std::string r;
  while ( *s ) r += toupper(*s++);
  return r;
}

std::string tolower(const std::string& str)
{
  const char* s = str.c_str();
  std::string r;
  while ( *s ) r += tolower(*s++);
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
  char buf[64];
  sprintf(buf, "%d", n);
  return std::string(buf);
}

std::string to_s(decimal_t n)
{
  char buf[64];
  sprintf(buf, "%g", n);
  return std::string(buf);
}

std::string to_s(bool f)
{
  return std::string(f? "#t" : "#f");
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
  s = s? s : "";
#ifdef BOHEM_GC
  return strcpy((char*) GC_MALLOC(1 + strlen(s)), s);
#else
  return strcpy((char*) malloc(1 + strlen(s)), s);
#endif
}

std::string encode_str(const char* s)
{
  // TODO: Make this table-based or something
  std::string r;

  for ( ; *s; ++s ) {
    switch ( *s ) {
    default:
      if ( isprint(*s) )
        r += *s; // printable, add as-is
      else {
        unsigned char v = static_cast<unsigned char>(*s);
        r += format("\\x%.*x;", sizeof(v)*8/4, v);
      }
      break;
    case '\n': r += "\\n"; break;
    case '\r': r += "\\r"; break;
    case '\t': r += "\\t"; break;
    case '\\': r += "\\"; break;
    }
  }

  return r;
}

// indefinite article
std::string indef_art(const std::string& s)
{
  return (isvowel(s[0])? "an " : "a ") + s;
}
