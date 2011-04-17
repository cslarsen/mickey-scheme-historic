#include <stdio.h>
#include <ctype.h>
#include <string>
#include <map>
#include "test.h"
#include "util.h"

enum type_t {
  NIL, INTEGER, CLOSURE, PAIR, SYMBOL, STRING, U8VECTOR, CONTINUATION
};

typedef struct cons_t* (*lambda_t)(const struct cons_t*);

struct environment_t {
    std::map<std::string, struct symbol_t*> symbols;
};

struct continuation_t {
};

struct closure_t {
  lambda_t function;
  environment_t* environment;
};

struct u8vector_t {
};

class symbol_t {
  symbol_t(const char* s) : name(toupper(s))
  {
  }

  symbol_t()
  {
  }

public:
  std::string name;

  static symbol_t* create_symbol(const char* s, environment_t* env)
  {
    std::string S = toupper(s);

    if ( env->symbols.find(S) == env->symbols.end() )
      env->symbols[S] = new symbol_t(S.c_str());

    return env->symbols[S];
   }
};

struct cons_t {
  type_t type;
  union {
    int integer;
    struct { cons_t *car, *cdr; };
    closure_t* closure;
    symbol_t* symbol;
    const char* string;
    u8vector_t* u8vector;
    continuation_t* continuation;
  };
};

cons_t* cons(cons_t* head, cons_t* tail = NULL)
{
  cons_t *p = new cons_t();
  p->type = PAIR;
  p->car = head;
  p->cdr = tail;
  return p;
}

cons_t* list(cons_t* head, cons_t* tail)
{
  return cons(head, cons(tail, NULL));
}

cons_t* atom(int n)
{
  cons_t *p = new cons_t();
  p->type = INTEGER;
  p->integer = n;
  return p;
}

environment_t globals;

cons_t* symbol(const char* s, environment_t *env = &globals)
{
  cons_t *p = new cons_t();
  p->type = SYMBOL;
  p->symbol = symbol_t::create_symbol(s, env);
  return p;
}

cons_t* integer(int n)
{
  cons_t *p = new cons_t();
  p->type = INTEGER;
  p->integer = n;
  return p;
}

std::string to_s(int n)
{
  char buf[32];
  sprintf(buf, "%d", n);
  return std::string(buf);
}

cons_t* car(cons_t* p)
{
  return ( p != NULL && p->type == PAIR ) ? p->car : NULL;
}

cons_t* cdr(cons_t* p)
{
  return ( p != NULL && p->type == PAIR ) ? p->cdr : NULL;
}

bool symbolp(cons_t* p)
{
  return p != NULL && p->type == SYMBOL;
}

bool atomp(cons_t* p)
{
  return p != NULL && (p->type == SYMBOL || p->type == INTEGER || p->type == STRING);
}

bool pairp(cons_t* p)
{
  return p != NULL && p->type == PAIR;
}

std::string sprint(cons_t* p, std::string& s)
{
  if ( p != NULL )
  switch ( p->type ) {
  default: case NIL: return "";
  case INTEGER: return s + to_s(p->integer);
  case CLOSURE: return s + "<closure>";
  case PAIR: {
    bool parens = pairp(car(p));
    return s + (parens? "(" : "") + sprint(p->car, s)
      + ((atomp(car(p)) && atomp(cdr(p))) ? " . " : (cdr(p) != NULL ? " " : ""))
      + sprint(p->cdr, s) + (parens? ")" : "");
    } break;
  case SYMBOL: return s + p->symbol->name;
  case STRING: return s + "\"" + p->string + "\"";
  case U8VECTOR: return s + "<u8vector>";
  case CONTINUATION: return s + "<continuation>";
  }

  return s;
}

std::string sprint(cons_t* p)
{
  std::string s;
  return sprint(p, s);
}

int main(int argc, char** argv)
{
  TEST_STREQ(sprint(cons(cons(symbol("one"), symbol("two")))), "(ONE . TWO)");
  TEST_STREQ(sprint(cons(cons(integer(1), integer(2)))), "(1 . 2)");
  TEST_STREQ(sprint(cons(cons(integer(0), cons(integer(1), integer(2))))), "(0 1 . 2)");
  TEST_STREQ(sprint(cons(cons(symbol("zero"), cons(symbol("one"), symbol("two"))))), "(ZERO ONE . TWO)");

  /*
   * Common Lisp: (cons 1 (cons 2 nil))
   * Scheme:      (cons 1 (cons 2 '()))
   */
  TEST_STREQ(sprint(cons(cons(integer(1), cons(integer(2), NULL)))), "(1 2)");

  // (cons 1 (cons 2 (cons 3 nil)))
  TEST_STREQ(sprint(cons(cons(integer(1), cons(integer(2), cons(integer(3), NULL))))), "(1 2 3)");

  results();
  return 0;
}
