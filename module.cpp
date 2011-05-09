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

#include "module.h"
#include "options.h"

void import(environment_t *e, named_function_t *p)
{
  if ( global_opts.verbose )
    printf("Importing ");

  for ( ; p->name && p->function; ++p ) {
    e->define(p->name, p->function);

    if ( global_opts.verbose )
      printf("%s ", p->name? p->name : "<?>");
  }

  if ( global_opts.verbose )
    printf("\n");
}
