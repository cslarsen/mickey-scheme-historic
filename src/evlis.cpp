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
#include "print.h"

static bool isdot(cons_t* p)
{
  return symbolp(p) && p->symbol->name() == ".";
}

cons_t* evlis(cons_t* p, environment_t* e)
{
  cons_t *r = list();

  for ( cons_t *end = r; pairp(p); p = cdr(p) ) {
    /*
     * We use *end so we don't have to use append()
     */
    end->car = eval(car(p), e);

    /*
     * If next symbol is a dot, skip it and continue
     * evaluating.  This makes it possible to use
     * dot notation as in (+ 1 . (2 . ()))
     */
    if ( isdot(cadr(p)) ) {
      p = cddr(p);
      end->cdr = evlis(car(p), e);
      break;
    }

    end->cdr = cons(nil());
    end = cdr(end);
  }

  return r;
}
