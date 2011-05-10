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

void assert_length(const cons_t* p, const size_t e)
{
  const size_t l = length(const_cast<cons_t*>(p));

  if ( e != l )
    throw std::runtime_error(format(
      "Function expects exactly %lu parameters but got %lu: `%s´",
        e, l, sprint(p).c_str()));
}

void assert_type(const enum type_t type, cons_t* p)
{
  if ( type_of(p) != type )
    throw std::runtime_error(format("Function expected a %s but got a %s: `%s´",
      to_s_type(type).c_str(),
      to_s_type(type_of(p)).c_str(),
      sprint(p).c_str()));
}

void assert_number(const cons_t* p)
{
  if ( !numberp(p) )
    throw std::runtime_error(format("Function expected a number but got a %s: `%s´",
      to_s_type(type_of(p)).c_str(),
      sprint(p).c_str()));
}
