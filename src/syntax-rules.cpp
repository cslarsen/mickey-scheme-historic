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

#include "cons.h"
#include "primops.h"
#include "syntax-rules.h"

extern "C" { // because I don't like name-mangling

/*
 * See R7RS 4.3.2 Pattern Language for examples on how to match
 * a syntax-rules pattern with code.
 *
 * NOTE: We do NOT support the full pattern language yet.  We only
 *       support simple patterns like (<pattern>) and (<pattern> ...)
 *       and the same for templates.
 *
 *       There is also a small problem with the environments.  We should
 *       capture free variables from the environment where the macro was
 *       DEFINED, and reuse that... Currently, we don't do that.
 *
 */
static bool syntax_match(cons_t *pat, cons_t *c, dict_t& map)
{
  for ( ; !nullp(pat); pat = cdr(pat) ) {
    cons_t *l = car(pat);
    cons_t *r = car(c);

    // Match REST of symbols (TODO: Fix this, it's not very correct)
    if ( l->symbol->name() == "..." ) {
      map["..."] = c;
      return true;
    }

    // Pattern is too long; no match
    if ( nullp(c) )
      return false;

    map[l->symbol->name()] = r;
    c = cdr(c);
  }

  // Check that pattern was fully matched
  return nullp(pat);
}

static cons_t* syntax_replace(dict_t &map, cons_t* p)
{
  cons_t *start = p;

  for ( ; !nullp(p); p = cdr(p) ) {
    if ( listp(car(p)) )
      p->car = syntax_replace(map, car(p));
    else if ( symbolp(car(p)) && map.count(car(p)->symbol->name()) )
      p->car = map[car(p)->symbol->name()];
  }

  return start;
}

cons_t* syntax_expand(cons_t *macro, cons_t *code, environment_t*)
{
  cons_t *rules = macro->syntax->transformer;

  // Go through all rules and find a match
  for ( cons_t *p = cdar(rules); !nullp(p); p = cdr(p) ) {
    cons_t *pattern = caar(p);
    cons_t *expansion = cadar(p);

    dict_t map;
    if ( syntax_match(pattern, code, map) )
      return syntax_replace(map, deep_copy(expansion));
  }

  // TODO: What if there's no match?
  return code;
}

cons_t* make_syntax(cons_t* body, environment_t* e)
{
  syntax_t *s = new syntax_t();
  s->transformer = body;
  s->environment = e;

  cons_t *r = new cons_t();
  r->type = SYNTAX;
  r->syntax = s;
  return r;
}

} // extern "C"
