/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the LGPL 2.1; see LICENSE            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include "cons.h"
#include "module.h"

extern named_function_t exports_import[];

cons_t* proc_import(cons_t*, environment_t*);
environment_t* import_library(const std::string& name);
