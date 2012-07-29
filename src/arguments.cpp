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

size_t arg_length(cons_t* p)
{
  /*
   * Pure variadic function, e.g. (lambda x (length x)) with
   * x not enclosed in parens.
   */
  if ( !nullp(p) && length(p)==1 && !symbolp(car(p)) )
    // TODO: Check if this really works, esp. the !symbolp part
    return 0;

  size_t count  = 0;

  while ( pairp(p) ) {
    p = cdr(p);
    ++count;
  }

  return count;
}

bool has_rest_args(cons_t* p)
{
  /*
   * We now use proper dot notation so that
   * function signatures are parsed either
   * as a pure list, e.g. (arg1 arg2 arg3)
   * or as a non-proper list that is not
   * terminated with a nil, e.g.
   * (arg1 arg2 . rest-args)
   */
  while ( pairp(p) )
    p = cdr(p);

  return !nullp(p);
}
