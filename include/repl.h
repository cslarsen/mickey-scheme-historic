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

#ifdef NO_EXCEPTIONS
# include "setjmp.h"
extern jmp_buf* jmpbuf_repl;
#endif

#include "cons.h"

int repl();
