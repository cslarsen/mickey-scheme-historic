#include <stdio.h>
#include <string>
#include "test.h"

#define TEST_STREQ(expr, expected) { test_streq(#expr, expr, expected); }

void test_streq(const std::string& code, const std::string& actual, const std::string& expected)
{
  test(actual == expected, code.c_str());

  if ( actual != expected ) {
    printf("  expected: %s\n", expected.c_str());
    printf("  actual  : %s\n", actual.c_str());
  }
}

enum type_t {
  NIL, INTEGER, CLOSURE, PAIR, SYMBOL, STRING, U8VECTOR, CONTINUATION
};

typedef struct cons_t* (*lambda_t)(const struct cons_t*);

struct environment_t {
};

struct continuation_t {
};

struct closure_t {
  lambda_t function;
  environment_t* environment;
};

struct u8vector_t {
};

struct cons_t {
  type_t type;
  union {
    int integer;
    struct { cons_t *car, *cdr; };
    closure_t* closure;
    const char* symbol;
    const char* string;
    u8vector_t* u8vector;
    continuation_t* continuation;
  };
};

cons_t* cons(cons_t* head, cons_t* tail)
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

cons_t* symbol(const char* s)
{
  cons_t *p = new cons_t();
  p->type = SYMBOL;
  p->symbol = s;
  return p;
}

std::string to_s(int n)
{
  char buf[32];
  sprintf(buf, "%d", n);
  return std::string(buf);
}

std::string sprint(cons_t* p, std::string& s)
{
  if ( p != NULL )
  switch ( p->type ) {
  default: case NIL: return "";
  case INTEGER: return s + to_s(p->integer);
  case CLOSURE: return s + "<closure>";
  case PAIR: return s + "(" + sprint(p->car, s) + " . " + sprint(p->cdr, s) + ")";
  case SYMBOL: return s + p->symbol;
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
  TEST_STREQ(sprint(cons(symbol("one"), symbol("two"))), "(one . two)");
  TEST_STREQ(sprint(cons(symbol("zero"), cons(symbol("one"), symbol("two")))), "(zero (one . two))");
  results();
  return 0;
}
