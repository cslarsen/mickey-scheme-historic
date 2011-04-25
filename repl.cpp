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
    dict_t::const_iterator i = env->symbols.begin();

    while ( i != env->symbols.end() ) {
      std::string n = (*i).first;
      r = append(cons(string(n.c_str())), r);
      ++i;
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

  // add some more definitions
  env->defun("exit", defun_quit);
  env->defun("run-tests", defun_run_tests);
  env->defun("list-globals", defun_list_globals);

  printf("Mickey Scheme (C) 2011 Christian Stigen Larsen\n");
  printf("\n");
  printf("Loaded %ld definitions\n", env->symbols.size());
  printf("Execute (exit [ code ]) to quit\n");
  printf("Execute (run-tests) to run tests\n");
  printf("Execute (list-globals) to list known definitions\n");

  printf("Definitions: %s\n", sprint(defun_list_globals(nil(), env)).c_str());

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
