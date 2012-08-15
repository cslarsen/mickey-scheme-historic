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

#include "cons.h"
#include "primops.h"
#include "module_load.h"
#include "assertions.h"
#include "exceptions.h"
#include "options.h"
#include "eval.h"
#include "file_io.h"

named_function_t exports_load[] = {
  {"load", proc_load, false},
  {NULL, NULL, false}
};

cons_t* proc_load(cons_t *args, environment_t *env)
{
  assert_length(args, 1, 2);
  assert_type(STRING, car(args));

  cons_t *filename = car(args);
  cons_t *env_spec = cadr(args);

  if ( !nullp(env_spec) ) {
    raise(runtime_exception(
      "Environment-specifiers not supported in (load)"));
  } else {
    // TODO: Use (interaction-environment)
  }

  // first try filename without include path
  std::string file = filename->string;

  // no cigar? try include path
  if ( !file_exists(file) )
    file = format("%s/%s", global_opts.include_path, filename->string);

  if ( !file_exists(file) ) {
    raise(runtime_exception(format(
      "Could not find file '%s' in any search paths", filename->string)));
  }

  // Set current filename, in case we need it for error reporting.
  const char* prev = global_opts.current_filename;
  global_opts.current_filename = file.c_str();

  // Parse and evaluate file.
  program_t *p = parse(slurp(open_file(file)), env);
  eval(cons(symbol("begin", p->globals), p->root), p->globals);

  // Restore filename.
  global_opts.current_filename = prev;
  return nil();
}
