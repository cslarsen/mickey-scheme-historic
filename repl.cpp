#include <stdio.h>

#ifdef USE_READLINE
# include <readline/readline.h>
# include <readline/history.h>
#endif

#include "mickey.h"
#include "cons.h"
#include "util.h"
#include "repl.h"
#include "tests.h"
#include "parser.h"
#include "print.h"
#include "eval.h"
#include "primitives.h"
#include "heap.h"
#include "backtrace.h"

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

#ifdef USE_READLINE
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
#else
char* readline(const char* prompt)
{
  static char buf[1024];
  buf[0] = '\0';

  printf("%s", prompt);
  fflush(stdout);

  if ( !fgets(buf, sizeof(buf), stdin) )
    return NULL;

  return *buf? buf : NULL;
}
#endif // USE_READLINE

void print_banner(environment_t* env)
{
  std::string readline_version =
  #ifdef USE_READLINE
    format("Readline %d.%d",
      (rl_readline_version & 0xFF00) >> 8, rl_readline_version & 0x00FF);
  #else
    "";
  #endif

  std::string boehm_version =
  #ifdef BOEHM_GC
    format("Boehm-Demers-Weiser GC %d.%d", GC_VERSION_MAJOR, GC_VERSION_MINOR);
  #else
    "";
  #endif

  printf("%-65s _\n", "");
  printf("%-65s  \\\n", VERSION);
  printf("%-65s  /\\\n", __VERSION__);
  printf("%-65s /  \\_\n", readline_version.c_str());
  printf("%-65s       \n", boehm_version.c_str());

  printf("Loaded %ld definitions\n", env->symbols.size());
  printf("Execute (exit [ code ]) to quit\n");
  printf("You can also (run-tests) and (list-globals)\n");
  printf("\n");

}

int repl()
{
  environment_t *env = new environment_t();
  global_env = env;
  load_default_defs(env);

  #ifdef USE_READLINE
  init_readline();
  #endif

  // add some more definitions
  env->defun("run-tests", defun_run_tests);
  env->defun("list-globals", defun_list_globals);

  print_banner(env);

  for(;;) {
    char *input;

    if ( (input = readline("mickey> ")) == NULL )
      break; // end of input stream

    if ( *trimr(input) == '\0' )
      continue; // empty command

    #ifdef USE_READLINE
    add_history(input);
    #endif

    try {
      program_t *p = parse(input, env);
      std::string s = sprint(eval(p));

      #ifdef USE_READLINE
      free(input);
      #endif

      if ( !s.empty() )
        printf("%s\n", s.c_str());
    }
    catch(const std::exception& e) {
      fprintf(stderr, "%s\n", e.what());
      backtrace();
      backtrace_clear();
    }
  }

  return 0;
}
