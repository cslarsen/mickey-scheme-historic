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

#include <sys/stat.h>
#include <string>
#include "file_io.h"

std::string slurp(FILE *f)
{
  std::string r;

  for ( int c; (c = fgetc(f)) != EOF; )
    r += c;

  return r;
}

bool file_exists(const char* s)
{
  struct stat st;
  return !stat(s, &st);
}
