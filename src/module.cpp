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
#include "libraries/scheme-base.h"
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
void import_defaults(environment_t *e)
{
  if ( !global_opts.empty_repl_env ) {
    merge(e, import_library("(scheme base)"));
    merge(e, import_library("(scheme cxr)"));
    merge(e, import_library("(scheme write)"));
    merge(e, import_library("(scheme char)"));
    merge(e, import_library("(scheme load)"));
    merge(e, import_library("(scheme repl)"));
  }
}
