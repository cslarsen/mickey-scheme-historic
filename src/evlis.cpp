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

#include "evlis.h"
#include "primops.h"
#include "eval.h"

cons_t* evlis(cons_t* p, environment_t* e)
{
  /*
   * Recursive version:
   *
   * return !pairp(p) ? nil() :
   *   cons(eval(car(p), e),
   *        evlis(cdr(p), e));
   */

  /*
   * Iterative version
   */
  cons_t *r = list(), *end = r;

  while ( pairp(p) ) {
    /*
     * Instead of
     *   r = append(r, cons(eval(car(p), e)));
     * we keep track of the end and do the below
     * procedure.  This avoids the costly append().
     */
    end->car = eval(car(p), e);
    end->cdr = cons(nil());
    end = cdr(end);
    p = cdr(p);
  }

  return r;
}
