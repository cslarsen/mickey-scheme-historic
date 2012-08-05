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

#include <stdio.h>
#include <string.h>

#ifdef USE_READLINE
# include <readline/readline.h>
# include <readline/history.h>
#endif

#include "mickey.h"
#include "options.h"
#include "cons.h"
#include "parser.h"

options_t global_opts;

void set_default(struct options_t* p, int argc, char** argv)
{
  p->verbose = false;
  p->read_stdin = false;
  p->include_path = ".";
  p->lib_path = ".";
  p->argc = argc;
  p->argv = argv;
  reset_for_programs(p);
}

void set_lib_path(struct options_t* p, const char* lib_path)
{
  p->lib_path = strdup(lib_path);
}

void reset_for_programs(struct options_t* p, const char* file)
{
  p->current_output_device = port_t(stdout).output().textual();
  p->current_input_device = port_t(stdin).input().textual();
  p->current_error_device = port_t(stderr).output().textual();
  p->current_filename = file;
}

// return true if rest of parameters are files
bool parse_option(const char* s, struct options_t* p)
{
  #define ARGHIT(_short, _long) (!strcmp(s, _short) || !strcmp(s, _long))

  if ( !strcmp(s, "--") )
    return true;
  else if ( !strncmp(s, "-I", 2) && strlen(s) > 2 ) {
    p->include_path = s+2;
  } else if ( !strcmp(s, "-") ) {
    // TODO: read from standard input
  } else if ( ARGHIT("-v", "--verbose") ) {
    p->verbose = true;
  } else if ( ARGHIT("-V", "--version") ) {
    version();
    exit(0);
  } else if ( ARGHIT("-h", "--help") ) {
    help(); 
    exit(0);
  } else if ( ARGHIT("-e", "--eval") ) {
    p->eval_next = true;
  } else {
    fprintf(stderr, "Unknown option: %s\n\n", s);
    help();
    exit(1);
  }

  return false;
}

void help()
{
  printf("Usage: mickey [ option(s) ] [ file(s) | - ]\n"
    "\n"
    "Options:\n"
    "  -             Read program from standard input\n"
    "  --            Rest of arguments are files, i.e. files can now begin with -\n"
    "  -e --eval     Execute expression and print result\n"
    "  -h --help     Print help\n"
    "  -I<path>      Set include path for (load).\n"
    "  -V --version  Print version\n"
    "  -v --verbose  Verbose operation\n"
    "\n");
}

void version()
{
  printf("%s\n", VERSION);

  #ifdef USE_READLINE
  printf("Using Readline %d.%d\n",
        (rl_readline_version & 0xFF00) >> 8,
         rl_readline_version & 0x00FF);
  #endif

  #ifdef BOEHM_GC
  printf("Using Boehm-Demers-Weiser GC %d.%d\n",
    GC_VERSION_MAJOR,
    GC_VERSION_MINOR);
  #endif

  printf("Compiler version: %s\n", __VERSION__);
}
