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

struct options_t
{
  bool verbose;
  bool read_stdin;
  bool eval_next;
  FILE* current_output_device;
  FILE* current_input_device;
  const char* current_filename;
  const char* include_path;
};

extern options_t global_opts;

void set_default(struct options_t*);
void reset_for_programs(struct options_t*, const char* file = NULL);
bool parse_option(const char* arg, struct options_t*);
void help();
void version();
