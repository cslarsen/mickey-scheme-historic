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

#include <stdexcept>
#include "assertions.h"
#include "primops.h"
#include "print.h"
#include "util.h"
#include "exceptions.h"

void assert_length(const cons_t* p, const size_t e)
{
  const size_t l = length(const_cast<cons_t*>(p));

  if ( e != l )
    raise(std::runtime_error(format(
      "Function expects exactly %lu parameters but got %lu: `%s´",
        e, l, sprint(p).c_str())));
}

void assert_length(const cons_t* p, const size_t min, const size_t max)
{
  const size_t l = length(const_cast<cons_t*>(p));

  if ( l<min || l>max )
    raise(std::runtime_error(format(
      "Function expects from %lu to %lu parameters got %lu: `%s´",
        min, max, l, sprint(p).c_str())));
}

void assert_length_min(const cons_t* p, const size_t min)
{
  const size_t l = length(const_cast<cons_t*>(p));

  if ( l<min )
    raise(std::runtime_error(format(
      "Function expects at least %lu parameters got %lu: `%s´",
        min, l, sprint(p).c_str())));
}

void assert_type(const enum type_t type, cons_t* p)
{
  type_t p_type = type_of(p);
  bool error = false;

  if ( type != PAIR )
    /*
     * If expected type is NOT a pair, then types must match.
     */
    error = (p_type != type);
  else
    /*
     * If expected type IS a pair, then we can submit either
     * a LIST or a VECTOR to a function.
     */
    error = !(p_type == VECTOR || p_type == PAIR);

  if ( error )
    raise(std::runtime_error(format("Function expected %s but got %s: `%s´",
      indef_art(to_s(type)).c_str(),
      indef_art(to_s(type_of(p))).c_str(),
      sprint(p).c_str())));
}

void assert_number(const cons_t* p)
{
  if ( !numberp(p) )
    raise(std::runtime_error(format("Function expected a number but got a %s: `%s´",
      to_s(type_of(p)).c_str(),
      sprint(p).c_str())));
}
