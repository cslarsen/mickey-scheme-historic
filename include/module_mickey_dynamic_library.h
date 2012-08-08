/*
 * Mickey Scheme
 *
 * Copyright (C) 2011-2012 Christian Stigen Larsen <csl@sublevel3.org>
 * http://csl.sublevel3.org                              _
 *                                                        \
 * Distributed under the modified BSD license.            /\
 * Please post bugfixes and suggestions to the author.   /  \_
 *
 */

#include "cons.h"
#include "module.h"

extern named_function_t exports_mickey_dynamic_library[];

cons_t* proc_dlclose(cons_t*, environment_t*);
cons_t* proc_dlerror(cons_t*, environment_t*);
cons_t* proc_dlopen(cons_t*, environment_t*);
cons_t* proc_dlsym(cons_t*, environment_t*);
cons_t* proc_dlsym_syntax(cons_t*, environment_t*);
