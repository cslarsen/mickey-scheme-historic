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
#include "module_assert.h"
#include "exceptions.h"

void execute(const char* file)
{
  TRY {
    environment_t *env = new environment_t();

    import(env, exports_base);
    import(env, exports_math);
    import(env, exports_assert);

    reset_for_programs(&global_opts, file);
    proc_load(cons(string(file)), env);
  }
  CATCH (const std::exception& e) {
    const char* file = global_opts.current_filename;
    bool has_file = file && strcmp(file, "-");

    fprintf(stderr, "Error%s%s: %s\n",
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
    environment_t *env = new environment_t();

    import(env, exports_base);
    import(env, exports_math);
    import(env, exports_assert);

    reset_for_programs(&global_opts, NULL);

    program_t *p = parse(s, env);
    p->root = proc_begin(p->root, env);
    printf("%s\n", sprint(eval(p->root, env)).c_str());
  }
  CATCH (const std::exception& e) {
    fprintf(stderr, "Error: %s\n", e.what());
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
