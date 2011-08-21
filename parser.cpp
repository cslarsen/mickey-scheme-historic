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
#include "raise.h"

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

static cons_t* parse_quote(const char* t, environment_t* env);
static cons_t* parse_quasiquote(const char* t, environment_t* env);
static cons_t* parse_unquote(const char* t, environment_t* env);

cons_t* parse_list(environment_t *env, bool quoting = false)
{
  cons_t *p = NULL;
  const char *t;

  while ( (t = get_token()) != NULL && *t != ')' ) {
    bool paren = (*t == '(');

    cons_t *add;

    if ( isquote(t) )
      add = parse_quote(t, env);
    else if ( isquasiquote(t) )
      add = parse_quasiquote(t, env);
    else if ( isunquote(t) )
      add = parse_unquote(t, env);
    else
      add = paren? parse_list(env) :
                   type_convert(t + paren, env);

    p = nullp(p)? cons(add) : append(p, cons(add));

    /* Added this to prevent the rest of the program to be
     * treated as being quoted. I.e., the following happened
     * before this:
     *
     * mickey> '(1 2 3) 4
     * translated to "(quote (1 2 3) 4)", which is wrong
     */
    if ( quoting )
      break;
  }

  return p;
}

cons_t* parse_quote(const char* t, environment_t* env)
{
  bool is_symbol = (t[1] != '\0');

  // replace "'<exp>" with "(quote <exp>)"
  cons_t *r = cons(symbol("quote", env), is_symbol ?
    cons(type_convert(t+1, env)) :
    parse_list(env, true));

  return r;
}

cons_t* parse_quasiquote(const char* t, environment_t* env)
{
  bool quoted_symbol = (t[1] != '\0');

  // replace "`<exp>" with "(quasiquote <exp>)"
  cons_t *r = cons(symbol("quasiquote", env),
    quoted_symbol ?
      cons(type_convert(t+1, env)) :
      parse_list(env, true));

  return r;
}

cons_t* parse_unquote(const char* t, environment_t* env)
{
  bool quoted_symbol = (t[1] != '\0');

  // replace ",<exp>" with "(unquote <exp>)"
  cons_t *r = cons(symbol("unquote", env),
    quoted_symbol ?
      cons(type_convert(t+1, env)) :
      parse_list(env, true));

  return r;
}

program_t* parse(const char *program, environment_t *env)
{
  set_source(program);

  if ( env == NULL )
    raise(std::runtime_error("parse: null environment"));

  program_t *p = new program_t();
  p->globals = env;
  p->root = parse_list(p->globals);
  return p;
}
