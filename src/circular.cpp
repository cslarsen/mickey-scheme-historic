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

#include "circular.h"
#include "primops.h"
#include "print.h"

/*
 * Detecting cycles using Floyd's algorithm,
 * http://en.wikipedia.org/wiki/Cycle_detection
 */
bool circularp(const cons_t* p)
{
  const cons_t *t = p, *h = p;

  do {
    t = cdr(t);
    h = cddr(h);
  } while ( t != h );

  return !nullp(t);
}
