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

extern named_function_t exports_mickey_environment[];

cons_t* proc_boundp(cons_t*, environment_t*);
cons_t* proc_env_assign(cons_t*, environment_t*);
cons_t* proc_env_assignablep(cons_t*, environment_t*);
cons_t* proc_env_bindings(cons_t*, environment_t*);
cons_t* proc_env_bound_names(cons_t*, environment_t*);
cons_t* proc_env_boundp(cons_t*, environment_t*);
cons_t* proc_env_eval(cons_t*, environment_t*);
cons_t* proc_env_has_parentp(cons_t*, environment_t*);
cons_t* proc_env_lookup(cons_t*, environment_t*);
cons_t* proc_env_parent(cons_t*, environment_t*);
cons_t* proc_envp(cons_t*, environment_t*);
cons_t* proc_make_environment(cons_t*, environment_t*);
cons_t* proc_the_environment(cons_t*, environment_t*);
