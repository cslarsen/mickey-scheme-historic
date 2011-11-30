/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the modified BSD license.            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include "print.h"
#include "util.h"

std::string sprint(const vector_t* v, std::string& s, bool escape);
std::string sprint(const bytevector_t* v, std::string& s, bool escape);

std::string sprint(const cons_t* p, std::string& s, bool escape)
{
  switch ( type_of(p) ) {
  case NIL:          return s;
  case BOOLEAN:      return s + to_s(p->boolean);
  case CHAR:         return s + to_s(p->character, escape);
  case DECIMAL:      return s + to_s(p->decimal);
  case INTEGER:      return s + to_s(p->integer);
  case CLOSURE:      return s + (escape? to_s(p->closure) : "");
  case SYMBOL:       return s + p->symbol->name();
  case STRING:       return s + (escape? "\"" + encode_str(p->string) + "\"" : p->string);
  case VECTOR:       return s + sprint(p->vector, s, escape);
  case BYTEVECTOR:   return s + sprint(p->bytevector, s, escape);
  case CONTINUATION: return s + (escape? to_s(p->continuation) : "");
  case SYNTAX:       return s + sprint(p->syntax->transformer, s, escape);
  case PAIR: {
    std::string head = sprint(car(p), s, escape);
    std::string tail = (atomp(cdr(p)) && !nullp(cdr(p)) ?
                        ". " : "") + sprint(cdr(p), s, escape);
    return s
      + (listp(car(p)) ? "(" : "")
      + head
      + (listp(car(p)) ? ")" : "")
      + (!tail.empty() ? " " : "") + tail;
  }}

  return s;
}

std::string sprint(const cons_t* p)
{
  std::string s;
  return sprint(listp(p) ? cons(p) : p, s, true);
}

std::string sprint(const program_t* p)
{
  return sprint(p->root);
}

std::string print(const cons_t* p)
{
  std::string s;
  return sprint(list(p) ? cons(p) : p, s, false);
}

std::string print(const program_t* p)
{
  return print(p->root);
}

std::string sprint(const vector_t* v, std::string& r, bool)
{
  const std::vector<cons_t*>& p = v->vector;
  std::string s;
  s += "#(";

  for ( std::vector<cons_t*>::const_iterator i = p.begin();
        i != p.end(); ++i )
  {
    if ( i != p.begin() ) s += " ";
    if ( listp(*i) ) s += "(";

    if ( nullp(*i) )
      s += to_s(*i);
    else
      s += sprint(*i, r, true);

    if ( listp(*i) ) s += ")";
  }

  s += ")";
  return s;
}

std::string sprint(const bytevector_t* v, std::string&, bool)
{
  const std::vector<uint8_t>& p = v->bytevector;
  std::string s;
  s += "#u8(";

  std::string space = "";

  for ( std::vector<uint8_t>::const_iterator i = p.begin();
        i != p.end(); ++i )
  {
    s += space + to_s(*i);
    space = " ";
  }

  s += ")";
  return s;
}
