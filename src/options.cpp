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
  p->warn = false;
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
