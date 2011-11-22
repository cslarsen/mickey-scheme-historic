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

#include <stdio.h>
#include <stdexcept>
#include "exceptions.h"
#include "util.h"

class open_file {
  FILE *f;
  open_file(const open_file&);
  open_file& operator=(const open_file&);

public:
  open_file(const char* name, const char* access = "rt") : f(fopen(name, access))
  {
    if ( f == NULL )
      raise(std::runtime_error(format("Could not open file: %s",  name)));
  }

  ~open_file()
  {
    fclose(f);
  }

  inline operator FILE*() const
  {
    return f;
  }
};

std::string slurp(FILE*);
bool file_exists(const char*);
