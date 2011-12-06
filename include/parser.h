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

#ifndef INC_MICKEY_PARSER_H
#define INC_MICKEY_PARSER_H

#include "cons.h"
#include "primops.h"

struct program_t {
  environment_t *globals;
  cons_t *root;
  long int parens;
};

program_t* parse(const char *program, environment_t *env);

#endif
