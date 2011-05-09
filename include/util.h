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

#ifndef INC_MICKEY_UTIL_H
#define INC_MICKEY_UTIL_H
#include <string>
#include "cons.h"
#endif

std::string to_s_int(int n);
std::string to_s_float(float n);
std::string to_s_bool(bool f);
std::string format(const char *fmt, ...);
std::string toupper(const std::string& s);

char* copy_str(const char* s);
char* trimr(char* s);
const char* skip_space(const char* s);

int empty(const char*);
bool char_in(char ch, const char* s);

char* decode_literal_string(const char* s);
std::string encode_str(const char* s);
