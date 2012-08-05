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
#include "module_import.h"
#include "module_write.h"
#include "module_base.h"
#include "module_math.h"
#include "module_process_context.h"
#include "options.h"

void import(environment_t *e, named_function_t *p, const char* lib_name)
{
  if ( global_opts.verbose && lib_name != NULL )
    fprintf(stderr, "Importing %s\n", lib_name);

  for ( ; p->name && p->function; ++p )
    e->define(p->name, p->function);
}

void load(environment_t *e, const std::string& path, const std::string& file)
{
  if ( global_opts.verbose )
    printf("Loading %s\n", std::string(path + "/" + file).c_str());

  proc_load(cons(string((path + "/" + file).c_str())), e);
}

/*
 * Add default libraries here.
 */
void import_defaults(environment_t *e, const char* lib_path)
{
  import(e, exports_import, NULL);

  import(e, exports_base, "(scheme base)");
  load(e, lib_path, "base.scm");

  import(e, exports_write, "(scheme write)");
  import(e, exports_math, "(scheme math)");
  import(e, exports_process_context, "(scheme process-context)");

  if ( global_opts.verbose )
    fprintf(stderr, "Importing (scheme char)\n");
  load(e, lib_path, "char.scm");

  if ( global_opts.verbose )
    fprintf(stderr, "Importing (scheme lazy)\n");
  load(e, lib_path, "lazy.scm");
}
