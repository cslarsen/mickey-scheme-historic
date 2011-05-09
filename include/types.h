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

bool isfloat(const char*);
bool isinteger(const char*);
bool isstring(const char*);
bool isatom(const char*);
bool isbool(const char*);
bool ischar(const char*);

float to_f(const char*);
int to_i(const char*);
bool to_b(const char*);
char to_char(const char*);
