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
#include "module_math.h"
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

void load(environment_t *e, const std::string& path, const std::string& file)
{
  if ( global_opts.verbose )
    printf("Loading %s\n", std::string(path + "/" + file).c_str());

  proc_load(cons(string((path + "/" + file).c_str())), e);
}

extern named_function_t exports_process_context[];

/*
 * Add default libraries here.
 */
void import_defaults(environment_t *e, const char* lib_path)
{
  import(e, exports_base);
  import(e, exports_math);
  import(e, exports_process_context);
  load(e, lib_path, "base.scm");
  load(e, lib_path, "char.scm");
  load(e, lib_path, "lazy.scm");
}
