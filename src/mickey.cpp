/*
 * Mickey Scheme
 *
 * Copyright (C) 2011 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *                                                          
 */

#include <libgen.h> // dirname
#include "options.h"
#include "repl.h"
#include "parser.h"
#include "eval.h"
#include "print.h"
#include "heap.h"
#include "backtrace.h"
#include "module.h"
/*
#include "module_import.h"
#include "module_base.h"
#include "module_math.h"
*/
#include "exceptions.h"

#define MICKEY_LIB "MICKEY_LIB"

void execute(const char* file)
{
  TRY {
    environment_t *env = null_import_environment();
    reset_for_programs(&global_opts, file);
    load(file, env);
  }
  CATCH (const std::exception& e) {
    const char* file = global_opts.current_filename;
    bool has_file = file && strcmp(file, "-");

    /*
     * Finish any unfinished printing before
     * writing errors.
     */
    fflush(stdout);

    fprintf(stderr, "\nError%s%s: %s\n",
      has_file? " in " : "",
      has_file? file : "",
      e.what());

    backtrace();
    backtrace_clear();
    exit(1);
  }
}

void execute_string(const char* s)
{
  TRY {
    environment_t *env = null_import_environment();
    reset_for_programs(&global_opts, NULL);

    program_t *p = parse(s, env);
    p->root = cons(symbol("begin", p->globals), p->root);
    printf("%s\n", sprint(eval(p->root, env)).c_str());
  }
  CATCH (const std::exception& e) {
    fprintf(stderr, "\nError: %s\n", e.what());
    backtrace();
    backtrace_clear();
    exit(1);
  }
}

int main(int argc, char** argv)
{
  bool rest_is_files = false; // used with option `--`
  bool run_repl = true;

  set_default(&global_opts, argc, argv);

  /*
   * Set library path using either environment variable or current working
   * directory.
   */
  if ( getenv(MICKEY_LIB) )
    set_lib_path(&global_opts, getenv(MICKEY_LIB));
  else
    set_lib_path(&global_opts, (std::string(dirname(argv[0])) + "/lib/").c_str());

  #ifdef BOEHM_GC
  GC_INIT();
  #endif

  for ( int n=1; n<argc; ++n ) {
    if ( global_opts.eval_next ) {
      execute_string(argv[n]);
      global_opts.eval_next = false;
      run_repl = false;
    } else if ( !rest_is_files && argv[n][0] == '-' ) {
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
