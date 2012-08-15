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

#include "cons.h"
#include "module.h"

extern named_function_t exports_base[];

// Some selected procedures

cons_t* proc_begin(cons_t*, environment_t*);
cons_t* proc_cond(cons_t*, environment_t*);
cons_t* proc_define(cons_t*, environment_t*);
cons_t* proc_define_syntax(cons_t*, environment_t*);
cons_t* proc_set_car(cons_t*, environment_t*);
cons_t* proc_set_cdr(cons_t*, environment_t*);
cons_t* proc_let(cons_t*, environment_t*);
cons_t* proc_letstar(cons_t*, environment_t*);
cons_t* proc_letrec(cons_t*, environment_t*);
cons_t* proc_display(cons_t*, environment_t*);
cons_t* proc_map(cons_t*, environment_t*);
cons_t* proc_vector(cons_t*, environment_t*);
cons_t* proc_case(cons_t*, environment_t*);
