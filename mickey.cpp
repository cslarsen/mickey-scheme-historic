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
    struct environment_t *outer;
    std::map<std::string, struct symbol_t*> symbols;

    environment_t() : outer(NULL)
    {
    }
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

cons_t* list(cons_t* head, cons_t* tail = NULL)
{
  if ( tail == NULL )
    return cons(head, tail);
  else
    return cons(head, cons(tail, NULL));
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

cons_t* caar(cons_t* p)
{
  return car(car(p));
}

cons_t* cadr(cons_t* p)
{
  return car(cdr(p));
}

cons_t* cddr(cons_t* p)
{
  return cdr(cdr(p));
}

cons_t* cdar(cons_t* p)
{
  return cdr(car(p));
}

type_t type_of(cons_t* p)
{
  return p == NULL ? NIL : p->type;
}

bool symbolp(cons_t* p)
{
  return type_of(p) == SYMBOL;
}

bool atomp(cons_t* p)
{
  type_t t = type_of(p);
  return t==SYMBOL || t==INTEGER || t==STRING;
}

bool integerp(cons_t* p)
{
  return type_of(p) == INTEGER;
}

bool nullp(cons_t* p)
{
  return type_of(p) == NIL;
}

bool pairp(cons_t* p)
{
  return type_of(p) == PAIR;
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
    return s
      + (parens? "(" : "")
      + sprint(p->car, s)
      + (parens? ")" : "")
      + (atomp(cdr(p)) ? " ." : "")
      + ( (atomp(car(p)) && !nullp(cdr(p)) && !pairp(cdr(p))) || // <= THIS
          (!nullp(cdr(p)) && !pairp(cadr(p))) ||                 // <=  IS
          (atomp(p) && pairp(cdr(p))) ||                         // <= VERY
          (integerp(car(p)) && pairp(cdr(p)))                    // <= MESSY! (and wrong)
            ? " " : "")
      + sprint(p->cdr, s);
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

const char* source = NULL;
const char* get_token()
{
  static char token[256];
  token[0] = '\0';

  // skip whitespace
  while ( isspace(*source) )
    ++source;

  // emit tokens "(" or ")"
  if ( *source == '(' || *source == ')' ) {
    token[0] = *source++;
    token[1] = '\0';
    return token;
  }

  // emit other token
  for ( char *t = token;
        *source && *source != ')' && !isspace(*source);
        *t = '\0' )
  {
    *t++ = *source++;
  }

  // emit NULL when finished
  return *token != '\0' ? token : NULL;
}

cons_t* append(cons_t *h, cons_t *t)
{
  if ( h == NULL )
    return t;
  else if ( !pairp(h) )
    return NULL; // error; try running "(append 1 nil)", should throw error
  else if ( car(h) == NULL )
    h->car = t;
  else if ( cdr(h) == NULL )
    h->cdr = t;
  else
    append(cdr(h), t);

  return h;
}

cons_t* parse_list()
{
  cons_t *p = NULL;
  const char *token;

  while ( (token = get_token()) != NULL ) {
    if ( *token == ')' )
      break;
    else if ( *token == '(' )
      p = append(p, list(symbol(token+1, &globals), parse_list()));
    else
      p = append(p, list(symbol(token, &globals)));
  }

  return p;
}

cons_t* parse(const char *program)
{
  source = program;
  return cdr(parse_list());
}

void test()
{
  TEST_STREQ(sprint(cons(integer(1), NULL)), "1");
  TEST_STREQ(sprint(cons(cons(integer(1)), NULL)), "(1)");
  TEST_STREQ(sprint(cons(cons(integer(1), integer(1)), NULL)), "(1 . 1)");

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

  // (cons 0 (cons (cons 1 (cons 2 nil)) nil))
  TEST_STREQ(sprint(cons(cons(integer(0), cons(cons(integer(1), cons(integer(2), NULL)), NULL)), NULL)), "(0 (1 2))");

  TEST_STREQ(sprint(cons(list(integer(1)))), "(1)");
  TEST_STREQ(sprint(cons(list(integer(1), integer(2)))), "(1 2)");
  TEST_STREQ(sprint(cons(list(integer(1), list(integer(2), integer(3))))), "(1 (2 3))");
  TEST_STREQ(sprint(cons(list(integer(1), list(integer(2), list(integer(3), integer(4)))))), "(1 (2 (3 4)))");
  TEST_STREQ(sprint(cons(list(list(integer(1), integer(2)), integer(3)))), "((1 2) 3)");

  // (cons 1 (cons 2 (list 3 4)))
  TEST_STREQ(sprint(cons(cons(integer(1), cons(integer(2), list(integer(3), integer(4)))))), "(1 2 3 4)");

  // (cons (list 1 2) (list 3 4))
  TEST_STREQ(sprint(cons(cons(list(integer(1), integer(2)), list(integer(3), integer(4))))), "((1 2) 3 4)");

  // (cons (cons 1 (cons 2 nil)) 3)
  TEST_STREQ(sprint(cons(cons(cons(integer(1), cons(integer(2))), integer(3)))), "((1 2) . 3)");

  // (cons (list 1 2) 3)
  TEST_STREQ(sprint(cons(cons(list(integer(1), integer(2)), integer(3)))), "((1 2) . 3)");

  // (append (list 1 2) (list (list 4 5)))
  TEST_STREQ(sprint(cons(append(list(integer(1), integer(2)), list(list(integer(4), integer(5)))))), "(1 2 (4 5))");

  // (append (list 1 2) (list 3 4))
  TEST_STREQ(sprint(cons(append(list(integer(1), integer(2)), list(integer(3), integer(4))))), "(1 2 3 4)");

  // (append (list 1) 2)
  TEST_STREQ(sprint(cons(append(list(integer(1)), integer(2)))), "(1 . 2)");

  // clisp: (cons (cons nil nil) nil), yields: ((NIL))
  TEST_STREQ(sprint(cons(cons(cons(NULL, NULL), NULL))), "(())");

  // (append nil (list 1 2))
  TEST_STREQ(sprint(cons(append(NULL, list(integer(1), integer(2))))), "(1 2)");

  TEST_STREQ(sprint(parse("(cons 1 2)")), "(CONS 1 2)");
  TEST_STREQ(sprint(parse("(+ (* 1 2) 3)")), "(+ (* 1 2) 3)");
  TEST_STREQ(sprint(parse("(1)")), "(1)");
  TEST_STREQ(sprint(parse("((1))")), "((1))");
  TEST_STREQ(sprint(parse("((1 2))")), "((1 2))");
  TEST_STREQ(sprint(parse("((1 2) 3)")), "((1 2) 3)");
  TEST_STREQ(sprint(parse("((a b) c)")), "((A B) C)");
  TEST_STREQ(sprint(parse("(a (b c) d)")), "(A (B C) D)");
  TEST_STREQ(sprint(parse("a")), "");

  results();
}

int main(int argc, char** argv)
{
  printf("Type :QUIT to quit\n");
  printf("Type :TEST to run tests\n");

  int no=0;
  char buf[1024];

  for(;;) {
    buf[0] = '\0';

    printf("%d> ", no++);
    fflush(stdout);

    if ( fgets(buf, sizeof(buf)-1, stdin) == NULL ) {
      printf("\n");
      break;
    }

    trimr(buf);

    if ( toupper(buf) == ":QUIT" ) break;
    if ( toupper(buf) == ":TEST" ) test();

    printf("%s\n", sprint(parse(buf)).c_str());
  }

  return 0;
}
