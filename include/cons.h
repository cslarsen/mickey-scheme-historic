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

typedef double decimal_t;

#ifndef INC_MICKEY_CONS_H
#define INC_MICKEY_CONS_H

#include <stdexcept>
#include <string>
#include <map>
#include <vector>
#include "util.h"
#include "heap.h"

// types taken from the PICOBIT scheme paper

enum type_t {
  NIL,
  BOOLEAN,
  CHAR,
  INTEGER,
  DECIMAL,
  CLOSURE,
  PAIR,
  SYMBOL,
  STRING,
  VECTOR,
  CONTINUATION,
  BYTEVECTOR,
  SYNTAX,
  PORT
};

typedef struct cons_t* (*lambda_t)(struct cons_t*, struct environment_t*);
typedef std::map<std::string, struct cons_t*> dict_t;
// TODO: Use hash_map and global string pointers

struct environment_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
  struct environment_t *outer;
  dict_t symbols;

  environment_t* extend();
  struct cons_t* lookup(const std::string& name) const;
  struct cons_t* lookup_or_throw(const std::string& name) const;
  struct cons_t* define(const std::string& name, lambda_t func);
  struct cons_t* define(const std::string& name, cons_t* body);
  environment_t* outmost();

private:
  environment_t() : outer(NULL)
  {
  }

  /*
   * Control construction
   */
  friend environment_t* null_environment(int);

  // Disabled functions
  environment_t(const environment_t&);
  environment_t& operator=(const environment_t&);
};

struct continuation_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
};

struct closure_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
  lambda_t function;
  environment_t* environment;
};

struct syntax_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
  cons_t* transformer;
  environment_t* environment;
};

struct vector_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
  std::vector<cons_t*> vector;

  vector_t()
  {
  }

  vector_t(const vector_t& v)
  {
    if ( this != &v )
      vector = v.vector;
  }

  vector_t(size_t size) : vector(size)
  {
  }

  vector_t(size_t size, cons_t* fill) : vector(size, fill)
  {
  }
};

struct bytevector_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
  std::vector<uint8_t> bytevector;

  bytevector_t()
  {
  }

  bytevector_t(const bytevector_t& v) : bytevector(v.bytevector)
  {
  }

  bytevector_t(size_t size) : bytevector(size)
  {
  }

  bytevector_t(size_t size, const uint8_t fill) : bytevector(size, fill)
  {
  }

  bytevector_t(const std::vector<uint8_t>& v) : bytevector(v)
  {
  }
};

class symbol_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
public:
  const std::string *n;

  symbol_t() : n(NULL)
  {
  }

  symbol_t(const symbol_t& s) : n(s.n)
  {
  }

  const std::string& name() const
  {
    return *n;
  }
};

const symbol_t* create_symbol(const std::string& s);

enum porttype_t {
  TEXTUAL_PORT,
  BINARY_PORT
};

class port_t
{
  FILE *f;
  char *s;
public:
  porttype_t port_type;
  bool writable, readable;

  port_t() :
    f(NULL),
    s(NULL),
    port_type(TEXTUAL_PORT),
    writable(false),
    readable(false)
  {
  }

  port_t(FILE* file) :
    f(file),
    s(NULL),
    port_type(TEXTUAL_PORT),
    writable(false),
    readable(false)
  {
  }

  port_t(const port_t& r)
  {
    if ( this != &r )
      memcpy(this, &r, sizeof(port_t));
  }

  port_t& textual()
  {
    port_type = TEXTUAL_PORT;
    return *this;
  }

  port_t& input()
  {
    readable = true;
    return *this;
  }

  port_t& output()
  {
    writable = true;
    return *this;
  }

  bool fileport() const
  {
    return f!=NULL && s==NULL;
  }

  bool stringport() const
  {
    return f==NULL && s!=NULL;
  }

  FILE* file() const
  {
    return f;
  }

  bool isopen() const
  {
    // file port
    if ( f != NULL )
      return ftell(f) != -1; // TODO: Is there a better way to check?

    // string port
    if ( s != NULL )
      return writable || readable;

    return false;
  }

  bool iswritable() const
  {
    return writable;
  }

  bool isreadable() const
  {
    return readable;
  }

  bool istextual() const
  {
    return port_type == TEXTUAL_PORT;
  }

  bool isbinary() const
  {
    return port_type == BINARY_PORT;
  }

  friend bool operator==(const port_t& l, const port_t& r)
  {
    return l.f == r.f && l.port_type == r.port_type &&
      l.writable == r.writable && l.readable == r.readable &&
      l.s == r.s;
  }

  port_t& operator=(const port_t& r)
  {
    if ( this != &r )
      memcpy(this, &r, sizeof(port_t));
    return *this;
  }

  void close()
  {
    if ( f != NULL ) {
      fclose(f);
      readable = writable = false;
    }

    if ( s != NULL ) {
      s = NULL;
      readable = writable = false;
    }
  }
};

/*
 * TODO: To cons_t, Add `marked` (for GC) and `mutable/immutable` (per spec)
 */

struct cons_t
 #ifdef BOEHM_GC
  : public gc
 #endif
{
  type_t type;
  union {
    bool boolean;
    char character;
    int integer;
    decimal_t decimal;
    struct { cons_t *car, *cdr; }; // pair
    closure_t* closure;
    syntax_t* syntax;
    const symbol_t* symbol;
    const char* string;
    vector_t* vector;
    bytevector_t* bytevector;
    continuation_t* continuation;
    port_t* port;
  };
};

// Should be in util, but cannot due to circular
// dependencies between cons.h and util.h (TODO: Fix that)
std::string encode_str(const char*);
std::string to_s(enum type_t);
std::string to_s(closure_t*);
std::string to_s(continuation_t*);
std::string to_s(vector_t*);
std::string to_s(bytevector_t*);
std::string to_s(port_t*);
std::string to_s(char, bool);
std::string to_s(struct cons_t *p);;
cons_t* deep_copy(const cons_t*);

/*
 * Merges the two environments by copying `b´ into `a´.
 * Returns number of symbols copied.
 */
int merge(environment_t *to, const environment_t *from);

#endif
