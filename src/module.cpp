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

#include <string>
#include "primops.h"
#include "module.h"
#include "module_base.h"
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

static void load(environment_t *e, const std::string& path, const std::string& file)
{
  proc_load(cons(string((path + "/" + file).c_str())), e);
}

void import_defaults(environment_t *e, const char* lib_path)
{
  load(e, lib_path, "base-list.scm");
  load(e, lib_path, "base-io.scm");
  load(e, lib_path, "base-string.scm");
}
