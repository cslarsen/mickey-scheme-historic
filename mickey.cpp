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

#include "options.h"
#include "repl.h"
#include "parser.h"
#include "eval.h"
#include "print.h"
#include "heap.h"
#include "backtrace.h"
#include "module_base.h"
#include "module_math.h"

void execute(const char* file)
{
  try {
    environment_t *env = new environment_t();

    import(env, exports_base);
    import(env, exports_math);

    reset_for_programs(&global_opts);
    global_opts.current_filename = file;

    proc_load(cons(string(file)), env);
  } catch (const std::exception& e) {
    const char* file = global_opts.current_filename;
    bool    has_file = file && strcmp(file, "-");

    fprintf(stderr, "Error%s%s: %s\n",
      has_file? " in " : "",
      has_file? file : "",
      e.what());

    backtrace();
    backtrace_clear();
    exit(1);
  }
}

int main(int argc, char** argv)
{
  bool rest_is_files = false; // used with option `--`
  bool run_repl = true;

  set_default(&global_opts);

  #ifdef BOEHM_GC
  GC_INIT();
  #endif

  if ( argc == 1 )
    return repl();

  for ( int n=1; n<argc; ++n ) {
    if ( !rest_is_files && argv[n][0] == '-' ) {
      if ( argv[n][1] == '\0' )
        execute("-"); // stdin
      else
        rest_is_files |= parse_option(argv[n], &global_opts);
    } else {
      execute(argv[n]); // file
      run_repl = false;
    }
  }

  if ( run_repl )
    repl();

  return 0;
}
