#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>
#include "cons.h"
#include "util.h"
#include "repl.h"
#include "tests.h"
#include "parser.h"
#include "print.h"
#include "eval.h"
#include "primitives.h"

cons_t* defun_list_globals(cons_t* p, environment_t *env)
{
  cons_t *r = NULL;

  do {
    for ( dict_t::const_iterator i =
      env->symbols.begin(); i != env->symbols.end(); ++i )
    {
      cons_t *s = list(symbol((*i).first.c_str(), env));
      r = append(s, r);
    }
  } while ( (env = env->outer) != NULL );

  return r;
}

cons_t* defun_quit(cons_t* p, environment_t*)
{
  if ( integerp(car(p)) )
    exit(car(p)->integer);
  else
    exit(0);
  return nil();
}

cons_t* defun_run_tests(cons_t*, environment_t*)
{
  run_tests();
  return nil();
}

int repl()
{
  char* input, prompt[1000];

  environment_t *env = new environment_t();
  load_default_defs(env);
  printf("Loaded %ld definitions\n", env->symbols.size());

  // add some more definitions
  env->defun("exit", defun_quit);
  env->defun("run-tests", defun_run_tests);
  env->defun("list-globals", defun_list_globals);

  printf("Added defs, loaded %ld definitions\n", env->symbols.size());
  printf("Execute (exit [ code ]) to quit\n");
  printf("Execute (run-tests) to run tests\n");
  printf("Execute (list-globals) to list known definitions\n");

  for(;;) {
    sprintf(prompt,"mickey> ");
    input=readline(prompt);

    if ( input == NULL )
      break;

// TODO: Add auto-completer, based on symbol table in globals
//    rl_bind_key('\t',rl_complete);
    add_history(input);

    trimr(input);

    if ( input[0] == '\0' )
      continue;

    try {
      program_t *p = parse(input, env);
      load_default_defs(env);

      std::string s = sprint(eval(p));

      if ( !s.empty() )
        printf("%s\n", s.c_str());
    }
    catch(const std::exception& e) {
      fprintf(stderr, "%s\n", e.what());
    }
  }

  return 0;
}
