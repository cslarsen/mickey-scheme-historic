/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the modified BSD license.            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include "arguments.h"
#include "primops.h"
#include "exceptions.h"
#include "print.h"

size_t arg_length(cons_t* args)
{
  size_t count  = 0;
  bool prev_dot = false;

  cons_t *p = args;

  // pure variadic function
  if ( !nullp(args) && length(args)==1 && !symbolp(car(args)) )
    return 0;

  for ( ; !nullp(p); p = cdr(p) ) {
    if ( symbolp(car(p)) && car(p)->symbol->name() == "." )
      prev_dot = true;
    else {
      if ( prev_dot ) {
        p = cdr(p); // so as not to break test below
        break;
      }
      ++count;
    }
  }

  if ( length(p) != 0 )
    raise(std::runtime_error("Invalid lambda signature: " + sprint(args)));

  return count;
}

bool has_rest_args(cons_t* p)
{
  return !listp(p) || (arg_length(p) < length(p));
}
