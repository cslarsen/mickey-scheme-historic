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

#ifndef INC_SCHEME_CHAR_H
#define INC_SCHEME_CHAR_H

#include "cons.h"
#include "module.h"

extern "C" {

extern named_function_t exports_char[];

cons_t* proc_char_alphabeticp(cons_t* p, environment_t*);
cons_t* proc_char_downcase(cons_t*, environment_t*);
cons_t* proc_char_lowercasep(cons_t* p, environment_t*);
cons_t* proc_char_numericp(cons_t* p, environment_t*);
cons_t* proc_char_upcase(cons_t* p, environment_t*);
cons_t* proc_char_uppercasep(cons_t* p, environment_t*);
cons_t* proc_char_whitespacep(cons_t* p, environment_t*);

};

#endif
