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

cons_t* defun_quit(cons_t* p)
{
  if ( integerp(car(p)) )
    exit(car(p)->integer);
  else
    exit(0);
  return nil();
}

cons_t* defun_run_tests(cons_t*)
{
  run_tests();
  return nil();
}

int repl()
{
  char* input, prompt[1000];

  environment_t globals;
  load_default_defs(&globals);

  defun(symbol_t::create_symbol("exit", &globals), defun_quit);
  defun(symbol_t::create_symbol("run-tests", &globals), defun_run_tests);

  printf("Loaded %ld definitions\n", globals.symbols.size());
  printf("Execute (exit [ code ]) to quit\n");
  printf("Execute (run-tests) to run tests\n");

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
      program_t *p = parse(input, &globals);
      load_default_defs(p->globals);

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
