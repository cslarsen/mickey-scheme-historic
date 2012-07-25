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

#ifndef INC_MICKEY_CIRCULAR_H
#define INC_MICKEY_CIRCULAR_H

#include "cons.h"

/*
 * Detects cycles in given list.
 *
 * Does not follow branches.
 */
bool circularp(const cons_t*);

#endif
