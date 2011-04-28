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
#include "heap.h"

// make env reachable by readline commands
static environment_t *global_env = NULL;

cons_t* defun_list_globals(cons_t*, environment_t *env)
{
  cons_t *r = NULL;

  for ( ; env != NULL; env = env->outer ) {
    dict_t::const_iterator i = env->symbols.begin();

    while ( i != env->symbols.end() ) {
      std::string n = (*i).first;
      r = append(cons(string(n.c_str())), r);
      ++i;
    }
  }

  return r;
}

cons_t* defun_run_tests(cons_t*, environment_t*)
{
  run_tests();
  return nil();
}

bool isprefix(const char* prefix, const char* fullstr)
{
  if ( strlen(prefix) > strlen(fullstr) )
    return false;

  while ( *prefix == *fullstr )
    ++prefix, ++fullstr;

  return *prefix == '\0';
}

char** auto_complete(const char *s, int start, int end)
{
  /*
   * Note that readline will free up stuff for us, so we
   * don't have to use GC_MALLOC / copy_str here, but can safely
   * use both malloc / strdup.
   */

  static size_t last_hit = 0;

  cons_t *all_commands = defun_list_globals(NULL, global_env);
  size_t count = 0;

  // Count number of hits
  for ( cons_t *p = all_commands; !nullp(p); p = cdr(p) )
    count += (stringp(car(p)) && isprefix(s, car(p)->string));

  if ( count <= 1 ) {
    last_hit = 0;
    return NULL;
  }

  // Return hits; readline will free for us
  char** hits = (char**) malloc((1+count)*sizeof(char*));
  char** hit = hits;

  size_t hitno = 0;
  for ( cons_t *p = all_commands; !nullp(p); p = cdr(p) )
    if ( stringp(car(p)) && isprefix(s, car(p)->string) ) {
      ++hitno;
      if ( hitno >= last_hit )
        *hit++ = strdup(car(p)->string);
    }

  last_hit = hitno % count;

  *hit = NULL;
  return hits;
}

char* readline_auto_completion(const char* s, int state)
{
  /*
   * Regarding GC, see comment in auto_complete()
   */

  static char** commands = NULL;
  static int idx = 0;

  // Start-state; build list of completion hits
  if ( state == 0 ) {
    // get all commands
    cons_t *all_commands = defun_list_globals(NULL, global_env);
    size_t count = 0;

    // count number of hits
    for ( cons_t *p = all_commands; !nullp(p); p = cdr(p) )
      count += (stringp(car(p)) && isprefix(s, car(p)->string));
  
    // build actual hits; readline will (hopefully!) free for us
    // (but TODO don't count on it)
    commands = (char**) malloc((1+count)*sizeof(char*));
    char** command = commands;

    for ( cons_t *p = all_commands; !nullp(p); p = cdr(p) ) {
      if ( stringp(car(p)) && isprefix(s, car(p)->string) )
          *command++ = strdup(car(p)->string);
    }

    *command = NULL;
    idx = 0;
  } else {
    // bring up next hit; don't know if this is right
    if ( commands[idx] != NULL )
      ++idx;
  }

  return commands[idx];
}

void init_readline()
{
  // for .inputrc customization
  rl_readline_name = strdup("mickey");

  /*
   * I think there is a bug in GNU Readline; the signature
   * expected by the rl_completion_entry_function is a function
   * returning int, but when it's used in completion_matches,
   * it is a function returning char*.
   */
  rl_completion_entry_function = (int(*)(const char*,int)) readline_auto_completion;

  // hitting TAB will attempt auto completion
  rl_bind_key('\t', rl_complete);
}

int repl()
{
  environment_t *env = new environment_t();
  global_env = env;
  load_default_defs(env);

  init_readline();

  // add some more definitions
  env->defun("run-tests", defun_run_tests);
  env->defun("list-globals", defun_list_globals);

  try {
    printf("%s\n", sprint(eval(parse("(display (version))", env))).c_str());
  } catch ( ... ) {
    // just in case we're working on eval
  }

  printf("Loaded %ld definitions\n", env->symbols.size());
  printf("Execute (exit [ code ]) to quit\n");
  printf("You can also (run-tests) and (list-globals)\n");
  printf("\n");

  for(;;) {
    char *input;

    if ( (input = readline("mickey> ")) == NULL )
      break; // end of input stream

    if ( *trimr(input) == '\0' )
      continue; // empty command

    add_history(input);

    try {
      program_t *p = parse(input, env);
      std::string s = sprint(eval(p));
      free(input);

      if ( !s.empty() )
        printf("%s\n", s.c_str());
    }
    catch(const std::exception& e) {
      fprintf(stderr, "%s\n", e.what());
    }
  }

  return 0;
}
