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

#include "cons.h"
#include "primops.h"
#include "module.h"
#include "module_mickey_environment.h"
#include "assertions.h"
#include "exceptions.h"
#include "options.h"

/*
 * Library exports
 */
named_function_t exports_mickey_environment[] = {
  {"bound?", proc_boundp, false},
  {NULL, NULL, false}
};

cons_t* proc_boundp(cons_t* p, environment_t* e)
{
  assert_length(p, 1);
  assert_type(SYMBOL, car(p));
  return boolean(e->lookup(car(p)->symbol->name()) != NULL);
}

