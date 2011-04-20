#include "test.h"
#include "tests.h"
#include "print.h"
#include "parser.h"
#include "types.h"
#include "eval.h"
#include "primitives.h"

static environment_t globals;

cons_t* symbol(const char* s)
{
  return symbol(s, &globals);
}

program_t* parse(const char *program)
{
  return parse(program, &globals);
}

void run_tests()
{
  load_default_defs(&globals);

  TEST_STREQ(format("a%sc%dd", "bb", 5), "abbc5d");
  TEST_STREQ(decode_literal_string("1 \\\"quo\\\" 2"), "1 \"quo\" 2");

  TEST_TRUE(isatom("a"));
  TEST_TRUE(isatom("ab"));
  TEST_TRUE(isatom("ab12"));
  TEST_TRUE(isatom("a1"));
  TEST_FALSE(isatom("1"));
  TEST_FALSE(isatom("1a"));
  TEST_FALSE(isatom("12b"));
  TEST_FALSE(isatom("-"));
  TEST_FALSE(isatom("a-"));
  TEST_FALSE(isatom("a1-c"));
  TEST_FALSE(isatom("a1c-"));

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
  TEST_STREQ(sprint(parse("(display (string-append \"Hello\" \", \" \"world!\"))")), "(DISPLAY (STRING-APPEND \"Hello\" \", \" \"world!\"))");
  TEST_STREQ(sprint(parse("(display \"Hello\\nworld!\")))")), "(DISPLAY \"Hello\\nworld!\")");
  TEST_STREQ(sprint(parse("a")), "A");
  TEST_STREQ(sprint(parse("(1 2 3) (4 5 6)")), "(1 2 3) (4 5 6)");
  TEST_STREQ(sprint(parse("(1 2 3)\r\n(4 5 6)")), "(1 2 3) (4 5 6)");
  TEST_STREQ(print(eval(parse("(->string 123)"))), "123");
  TEST_STREQ(print(eval(parse("(->string (list 1 2 (list 3 4)))"))), "(1 2 (3 4))");

  TEST_STREQ(print(eval(parse("(display 123)"))), ""); // prints to console
  TEST_STREQ(print(eval(parse("(display (+ 3 3 5))"))), ""); // prints to console
  TEST_STREQ(print(eval(parse("(display \"hello\")"))), ""); // prints to console
  TEST_STREQ(print(eval(parse("(+ 0)"))), "0");
  TEST_STREQ(print(eval(parse("(+ 1)"))), "1");
  TEST_STREQ(print(eval(parse("(+ 0 1)"))), "1");
  TEST_STREQ(print(eval(parse("(+ 1 2)"))), "3");
  TEST_STREQ(print(eval(parse("(+ 1 2 3)"))), "6");
  TEST_STREQ(print(eval(parse("(+ -1)"))), "-1"); 
  TEST_STREQ(print(eval(parse("(+ 5 -2 3)"))), "6"); 
  TEST_STREQ(print(eval(parse("(+ -1 -3 -7)"))), "-11"); 
  TEST_STREQ(print(eval(parse("(+ (* 5 5) 4)"))), "29"); 
  TEST_STREQ(print(eval(parse("(+ (* 5 5) 4 12)"))), "41"); 
  TEST_STREQ(print(eval(parse("(string-append \"one\" \"two\" \"three\")"))), "onetwothree");

  results();
}
