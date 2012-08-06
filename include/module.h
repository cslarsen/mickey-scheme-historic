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

#ifndef INC_MICKEY_MODULE_H
#define INC_MICKEY_MODULE_H

#include "cons.h"

struct named_function_t {
  const char* name;
  lambda_t function;
};

void import(environment_t*, named_function_t*, const std::string& lib_name = "");
void import_defaults(environment_t*, const std::string& lib_path);
void load(const std::string& file, environment_t*);

#endif
