/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include <string>
#include "primops.h"
#include "eval.h"
#include "file_io.h"
#include "module.h"
#include "module_import.h"
#include "module_base.h"
#include "module_math.h"
#include "options.h"

void import(environment_t *e,
            named_function_t *p,
            const std::string& lib_name)
{
  if ( global_opts.verbose )
    fprintf(stderr, "Importing internal library %s\n",
        lib_name.c_str());

  for ( ; p->name && p->function; ++p )
    e->define(p->name, p->function, p->syntactic);
}

void load(const std::string& file, environment_t* target)
{
  if ( global_opts.verbose )
    fprintf(stderr, "Loading file %s\n", file.c_str());

  program_t *p = parse(slurp(open_file(file)), target);
  eval(cons(symbol("begin", p->globals),
            p->root), p->globals);
}

/*
 * Add default libraries here.
 *
 */
void import_defaults(environment_t *e, const std::string& lib_path)
{
  if ( !global_opts.empty_repl_env ) {
    import(e, exports_base, "(scheme base)");
    load(lib_path + "/scheme/base.scm", e);
    load(lib_path + "/scheme/write.scm", e);
    load(lib_path + "/scheme/char.scm", e);
    load(lib_path + "/scheme/load.scm", e);
  }
}
