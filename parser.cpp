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

#include <stdexcept>
#include "parser.h"
#include "types.h"
#include "util.h"
#include "print.h"

cons_t* type_convert(const char* token, environment_t* env)
{
  if ( isfloat(token) )
    return decimal(to_f(token));

  if ( isinteger(token) )
    return integer(to_i(token));

  if ( isstring(token) )
    return string(decode_literal_string(token));

  if ( isatom(token) )
    return symbol(token, env);

  if ( isbool(token) )
    return boolean(to_b(token));

  if ( ischar(token) )
    return character(to_char(token));

  if ( !token || !*token )
    return nil();

  // probably a function called "+" or something
  return symbol(token, env);
}

cons_t* parse_list(environment_t *env)
{
  cons_t *p = NULL;
  const char *t;

  while ( (t = get_token()) != NULL && *t != ')' ) {
    bool paren = (*t == '(');

    cons_t *add = paren? parse_list(env) :
                         type_convert(t + paren, env);

    p = nullp(p)? cons(add) : append(p, cons(add));
  }

  return p;
}

program_t* parse(const char *program, environment_t *env)
{
  set_source(program);

  if ( env == NULL )
    throw std::runtime_error("parse: null environment");

  program_t *p = new program_t();
  p->globals = env;
  p->root = parse_list(p->globals);
  return p;
}
